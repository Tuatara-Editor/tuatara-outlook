;;;; pst-read.lisp --- Outlook mailboxes (.pst, .ost) read: their folders, messages, recipients and attachments.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mailbox is read as Microsoft's [MS-PST] lays it out (section numbers
;;;; below are of revision 11.2), in both its formats: the Unicode one of
;;;; Outlook 2003 and later and the ANSI one of Outlook 97 to 2002, its data
;;;; blocks as they are, permuted (NDB_CRYPT_PERMUTE) or cyclic
;;;; (NDB_CRYPT_CYCLIC). The node database (NDB) is read first: the header,
;;;; then the two B-trees, of nodes and of blocks, walked once into sorted
;;;; vectors. A node's data and the nodes within it are then read on demand,
;;;; a block at a time, from the file, which is never read whole nor written
;;;; to. On them the lists, tables and properties (LTP): a heap, B-trees on
;;;; it (of any depth), property contexts and table contexts. On those the
;;;; messaging layer: the message store, the folders, the messages with their
;;;; recipients and attachments, a message attached read as one, and the map
;;;; of named properties.
;;;;
;;;; A mailbox from elsewhere may be damaged or made to harm its reader. Every
;;;; offset and size is checked against the file and against what holds it;
;;;; every B-tree and tree of blocks is walked with each page or block seen
;;;; once and to a depth; a check (CRC) that does not match is noted, not
;;;; fatal unless what it guards cannot be used. What cannot be read raises a
;;;; PST-UNREADABLE saying why, which the caller turns into a message listed
;;;; as unreadable, never one left out.

(in-package #:outlook)

;;; Failures and notes

(define-condition pst-unreadable (error)
  ((reason :initarg :reason :reader pst-unreadable-reason))
  (:report (lambda (condition stream) (write-string (pst-unreadable-reason condition) stream)))
  (:documentation "Part of a mailbox that cannot be read: REASON says why, in words."))

(defun pst-fail (format &rest arguments)
  "Signal a PST-UNREADABLE whose reason is FORMAT with ARGUMENTS."
  (error 'pst-unreadable :reason (apply #'format nil format arguments)))

(defvar *pst-notes* nil
  "While a message is read, a list in a cons of what was noted of it that did
not stop it being read (a check that did not match, a value left out), the
latest first; NIL otherwise.")

(defun pst-note (file format &rest arguments)
  "Note FORMAT with ARGUMENTS of the mailbox FILE (a PST-FILE): in the notes
of the message being read (*PST-NOTES*) when there is one, and among FILE's
problems, a kind of problem counted once said."
  (let ((text (apply #'format nil format arguments)))
    (when *pst-notes* (push text (car *pst-notes*)))
    (when file (pst-problem file text))))

;;; The file

(defvar *pst-budget* nil
  "While a message is read (PST-READ-MESSAGE), what is left for it, a cons:
the octets of blocks it may still read, counting a block each time it is
read, its attachments and messages attached included; and the messages
attached it may still read, however deep. NIL otherwise.")

(defvar *pst-message-budget* nil
  "The most octets of blocks a message is read with, NIL for
*OUTLOOK-LARGEST-FILE*: the reader shut in is given less, from the memory it
may take (MAILBOX-MESSAGE-BUDGET), so that a message stops at its budget
before memory runs out.")

(defconstant +pst-most-attached-messages+ 256
  "The most messages attached a message is read with, counted across all its
attachments and theirs: those past them are listed as not read.")

(defconstant +pst-most-levels+ 8
  "The deepest B-tree of the node database read: the root's level at most
this. A mailbox of 50 GB needs four.")

(defconstant +pst-most-heap-levels+ 8
  "The most index levels a B-tree on a heap ([MS-PST] 2.3.2) is read with.")

(defconstant +pst-most-problems+ 100
  "The most problems of one mailbox kept in words; the rest are counted.")

(define-record (pst-file (:constructor %make-pst-file (path stream size)) (:copier nil))
  "A mailbox open for reading at PATH through STREAM, SIZE octets. FORMAT is
:unicode or :ansi; CRYPT how its data blocks are encoded. NODES and BLOCKS are
its two B-trees' leaves. NAMES is the map of named properties once read.
PROBLEMS is what was found wrong, in words, with PROBLEM-COUNT how many."
  (path nil :documentation "The path of the mailbox PST-FILE is open at.")
  (stream nil :documentation "The stream PST-FILE is read through.")
  (size nil :documentation "The octets in PST-FILE.")
  (format nil :documentation "The format of PST-FILE, :unicode or :ansi.")
  (crypt nil :documentation "How the data blocks of PST-FILE are encoded: 0 as they are, 1 permuted, 2 cyclic.")
  (nodes nil
         :documentation "The leaves of the node B-tree of PST-FILE as four vectors in a list, (NIDS DATAS SUBS PARENTS),
sorted by node number (NID).")
  (blocks nil
          :documentation "The leaves of the block B-tree of PST-FILE as three vectors, (BIDS OFFSETS SIZES), sorted by
block identifier (BID).")
  (names nil
         :documentation "The map of named properties of PST-FILE once read (PST-NAMES), a table from each property
number to (GUID . NAME).")
  (names-failure nil :documentation "Why the map of named properties of PST-FILE could not be read, or NIL.")
  (problems '()
            :documentation "What was found wrong in PST-FILE, in words, the latest first, at most +PST-MOST-PROBLEMS+.")
  (problem-count 0 :documentation "How many problems were found in PST-FILE.")
  (seen (make-hash-table :test 'equal) :documentation "The kinds of problem of PST-FILE said already."))

(defun pst-problem (file text)
  "Note TEXT among the problems of the mailbox FILE, once for each text: the
first +PST-MOST-PROBLEMS+ kept in words, the rest only counted."
  (unless (gethash text (pst-file-seen file))
    (when (<= (incf (pst-file-problem-count file)) +pst-most-problems+)
      (setf (gethash text (pst-file-seen file)) t)
      (push text (pst-file-problems file)))))

(defun pst-read-at (file at count)
  "COUNT octets of the mailbox FILE from the offset AT, a new vector; a
PST-UNREADABLE when any lies outside the file."
  (when (or (minusp at) (minusp count) (> (+ at count) (pst-file-size file)))
    (pst-fail "~:d octets at offset ~:d lie past the end of the file (~:d octets)" count at (pst-file-size file)))
  (let ((octets (pst-octets count)) (stream (pst-file-stream file)))
    (file-position stream at)
    (unless (= count (read-sequence octets stream))
      (pst-fail "the file ended before offset ~:d" (+ at count)))
    octets))

(defun pst-number-at (octets at count)
  "The unsigned integer in COUNT octets of OCTETS from AT, least significant
first; a PST-UNREADABLE when they lie past OCTETS' end."
  (or (and (<= 0 at) (<= (+ at count) (length octets)) (octets-integer octets at count))
      (pst-fail "a number at ~d lies past the end of what holds it (~d octets)" at (length octets))))

(defun pst-unicode-p (file)
  "True when the mailbox FILE is of the Unicode format, else of the ANSI one."
  (eq (pst-file-format file) :unicode))

(defun pst-word (file)
  "The octets of a node number, block identifier or offset in the mailbox
FILE's node database: eight in the Unicode format, four in the ANSI one."
  (if (pst-unicode-p file) 8 4))

(defun pst-block-room (file)
  "The most octets of data a block of the mailbox FILE holds: 8,176 in the
Unicode format, 8,180 in the ANSI one ([MS-PST] 2.2.2.8)."
  (if (pst-unicode-p file) 8176 8180))

(defun pst-read-header (file)
  "Read the header of the mailbox FILE ([MS-PST] 2.2.2.6): its format, how
its blocks are encoded, and where the roots of its two B-trees are, returned
as two conses (BID . OFFSET), of the nodes' and of the blocks'. A mailbox
not of the formats [MS-PST] gives, the 4 KB pages of newer offline mailboxes
among them, is refused saying so."
  (let* ((header (pst-read-at file 0 (min 564 (pst-file-size file))))
         (version (and (>= (length header) 12) (octets-integer header 10 2))))
    (unless (and (>= (length header) 512) (octets-at-p header "!BDN" 0))
      (pst-fail "it is not an Outlook mailbox: it does not begin as one"))
    (cond ((member version '(14 15)) (setf (pst-file-format file) :ansi))
          ((member version '(23 24 25 26 27 28 29 30 31 32 33 34 35)) (setf (pst-file-format file) :unicode))
          ((>= version 36)
           (pst-fail "it is an offline mailbox of Outlook 2013 or later, with pages of 4 KB (format version ~d), ~
                      which [MS-PST] does not describe and which is not read"
                     version))
          (t (pst-fail "its format version, ~d, is none that [MS-PST] describes" version)))
    (unless (= (octets-integer header 4 4) (pst-crc header :start 8 :end 479))
      (pst-problem file "the header's first check (CRC) does not match"))
    (if (pst-unicode-p file)
        (progn
          (when (< (length header) 564) (pst-fail "the header is cut short"))
          (unless (= (octets-integer header 524 4) (pst-crc header :start 8 :end 524))
            (pst-problem file "the header's full check (CRC) does not match"))
          (setf (pst-file-crypt file) (aref header 513))
          (values (cons (octets-integer header 216 8) (octets-integer header 224 8))
                  (cons (octets-integer header 232 8) (octets-integer header 240 8))))
        (progn
          (setf (pst-file-crypt file) (aref header 461))
          (values (cons (octets-integer header 184 4) (octets-integer header 188 4))
                  (cons (octets-integer header 192 4) (octets-integer header 196 4)))))))

;;; The node database: pages and the two B-trees

(defun pst-read-page (file ib bid ptype)
  "The 512-octet page of the mailbox FILE at the offset IB, said to have the
identifier BID and to be of PTYPE ([MS-PST] 2.2.2.7.1), checked: its type and
identifier (a PST-UNREADABLE when wrong), its signature and check (CRC),
noted when wrong."
  (let* ((page (pst-read-at file ib 512))
         (unicode (pst-unicode-p file))
         (trailer (if unicode 496 500))
         (own (if unicode (octets-integer page 504 8) (octets-integer page 504 4)))
         (crc (if unicode (octets-integer page 500 4) (octets-integer page 508 4))))
    (unless (and (= ptype (aref page trailer)) (= ptype (aref page (1+ trailer))))
      (pst-fail "the page at offset ~:d is not of the kind expected (~x, not ~x)" ib (aref page trailer) ptype))
    (unless (= (logandc2 own 1) (logandc2 bid 1))
      (pst-fail "the page at offset ~:d is page ~x, not ~x" ib own bid))
    (unless (= crc (pst-crc page :end trailer))
      (pst-problem file "the check (CRC) of a page of the B-trees does not match"))
    (unless (= (octets-integer page (+ trailer 2) 2) (pst-signature ib bid))
      (pst-problem file "the signature of a page of the B-trees does not match"))
    page))

(defun pst-walk-btree (file root ptype leaf)
  "Walk the B-tree of pages of PTYPE (#x81 the nodes', #x80 the blocks'),
whose root is ROOT, (BID . OFFSET), of the mailbox FILE, calling LEAF with a
leaf page and the offset of each of its entries, in the order of their
keys. Each page is read once, its level one less than its parent's, the
root's at most +PST-MOST-LEVELS+. A page that cannot be read is noted, with
what lies below it left out; a root that cannot be read is a
PST-UNREADABLE."
  (let* ((unicode (pst-unicode-p file))
         (area (if unicode 488 496))
         (entry-size (if (= ptype #x81) (if unicode 32 16) (if unicode 24 12)))
         (link-size (if unicode 24 12))
         (word (pst-word file))
         (seen (make-hash-table))
         (stack (list (list (cdr root) (car root) nil))))
    (loop while stack
          do (destructuring-bind (ib bid level) (pop stack)
               (handler-case
                   (progn
                     (when (gethash ib seen) (pst-fail "the B-tree goes round: the page at ~:d is reached again" ib))
                     (setf (gethash ib seen) t)
                     (let* ((page (pst-read-page file ib bid ptype))
                            (count (aref page area))
                            (width (aref page (+ area 2)))
                            (own (aref page (+ area 3))))
                       (cond ((and level (/= own level))
                              (pst-fail "a page at ~:d is at level ~d, not ~d" ib own level))
                             ((> own +pst-most-levels+)
                              (pst-fail "the B-tree is ~d levels deep, more than ~d" own +pst-most-levels+)))
                       (unless (= width (if (zerop own) entry-size link-size))
                         (pst-fail "the page at ~:d has entries of ~d octets, not ~d"
                                   ib width (if (zerop own) entry-size link-size)))
                       (when (> (* count width) area)
                         (pst-fail "the page at ~:d says it holds ~d entries, more than fit" ib count))
                       (if (zerop own)
                           (dotimes (i count) (funcall leaf page (* i width)))
                           (loop for i from (1- count) downto 0
                                 for at = (* i width)
                                 do (push (list (octets-integer page (+ at word word) word)
                                                (octets-integer page (+ at word) word)
                                                (1- own))
                                          stack)))))
                 (pst-unreadable (condition)
                   (when (eql ib (cdr root)) (error condition))
                   (pst-problem file (format nil "a page of the ~:[blocks'~;nodes'~] B-tree could not be read (~a): ~
                                                  what it holds is missing"
                                             (= ptype #x81) condition))))))))

(defun pst-sorted-columns (columns)
  "COLUMNS, a list of vectors of the same length, the first the keys, as
simple vectors sorted by the keys, an entry whose key an earlier one has
left out; returns them as a list and, as a second value, how many were left
out."
  (let* ((keys (first columns))
         (count (length keys))
         (order (let ((order (make-array count)))
                  (dotimes (i count order) (setf (aref order i) i))))
         (sorted (loop for i from 1 below count always (< (aref keys (1- i)) (aref keys i)))))
    (unless sorted
      (setf order (stable-sort order #'< :key (lambda (i) (aref keys i)))))
    (let ((kept (if sorted
                    order
                    (coerce (loop for k across order for i from 0
                                  unless (and (plusp i) (= (aref keys k) (aref keys (aref order (1- i))))) collect k)
                            'simple-vector))))
      (values (mapcar (lambda (column) (map 'simple-vector (lambda (k) (aref column k)) kept)) columns)
              (- count (length kept))))))

(defun pst-read-btrees (file nodes-root blocks-root)
  "Read the node and block B-trees of the mailbox FILE, rooted at NODES-ROOT
and BLOCKS-ROOT, into FILE's NODES and BLOCKS (PST-FILE), each sorted by its
keys; keys out of order or given twice are noted."
  (let ((unicode (pst-unicode-p file))
        (nids (make-array 1024 :adjustable t :fill-pointer 0)) (datas (make-array 1024 :adjustable t :fill-pointer 0))
        (subs (make-array 1024 :adjustable t :fill-pointer 0))
        (parents (make-array 1024 :adjustable t :fill-pointer 0))
        (bids (make-array 1024 :adjustable t :fill-pointer 0)) (ibs (make-array 1024 :adjustable t :fill-pointer 0))
        (cbs (make-array 1024 :adjustable t :fill-pointer 0)))
    (pst-walk-btree file nodes-root #x81
                    (lambda (page at)
                      (if unicode
                          (progn (vector-push-extend (octets-integer page at 4) nids)
                                 (vector-push-extend (octets-integer page (+ at 8) 8) datas)
                                 (vector-push-extend (octets-integer page (+ at 16) 8) subs)
                                 (vector-push-extend (octets-integer page (+ at 24) 4) parents))
                          (progn (vector-push-extend (octets-integer page at 4) nids)
                                 (vector-push-extend (octets-integer page (+ at 4) 4) datas)
                                 (vector-push-extend (octets-integer page (+ at 8) 4) subs)
                                 (vector-push-extend (octets-integer page (+ at 12) 4) parents)))))
    (pst-walk-btree file blocks-root #x80
                    (lambda (page at)
                      (let ((word (if unicode 8 4)))
                        (vector-push-extend (logandc2 (octets-integer page at word) 1) bids)
                        (vector-push-extend (octets-integer page (+ at word) word) ibs)
                        (vector-push-extend (octets-integer page (+ at word word) 2) cbs))))
    (multiple-value-bind (columns dropped) (pst-sorted-columns (list nids datas subs parents))
      (when (plusp dropped) (pst-problem file (format nil "~:d nodes are given twice in the nodes' B-tree" dropped)))
      (setf (pst-file-nodes file) columns))
    (multiple-value-bind (columns dropped) (pst-sorted-columns (list bids ibs cbs))
      (when (plusp dropped) (pst-problem file (format nil "~:d blocks are given twice in the blocks' B-tree" dropped)))
      (setf (pst-file-blocks file) columns))))

(defun pst-search (keys key)
  "The index of KEY in KEYS, a simple vector sorted in ascending order, or NIL."
  (declare (type simple-vector keys))
  (let ((low 0) (high (1- (length keys))))
    (loop while (<= low high)
          do (let* ((middle (floor (+ low high) 2)) (here (svref keys middle)))
               (cond ((< here key) (setf low (1+ middle)))
                     ((> here key) (setf high (1- middle)))
                     (t (return middle)))))))

(defun pst-find-node (file nid)
  "The node NID of the mailbox FILE as the nodes' B-tree gives it: its data
tree's and its subnode tree's BIDs and its parent's NID, three values; NIL
when it has no such node."
  (destructuring-bind (nids datas subs parents) (pst-file-nodes file)
    (let ((index (pst-search nids nid)))
      (and index (values (svref datas index) (svref subs index) (svref parents index))))))

(defun pst-open (path)
  "The mailbox at PATH open for reading (a PST-FILE): its header read and its
two B-trees walked, its map of named properties read (PST-NAMES); a
PST-UNREADABLE saying why when it cannot be read. The caller closes it
\(PST-CLOSE)."
  (let ((stream (open path :element-type '(unsigned-byte 8))))
    (handler-bind ((error (lambda (condition) (declare (ignore condition)) (close stream))))
      (let ((file (%make-pst-file path stream (file-length stream))))
        (multiple-value-bind (nodes blocks) (pst-read-header file)
          (unless (member (pst-file-crypt file) '(0 1 2))
            (pst-fail "its blocks are encoded in a way [MS-PST] does not describe (~d)" (pst-file-crypt file)))
          (pst-read-btrees file nodes blocks))
        (pst-names file)
        file))))

(defun pst-close (file)
  "Close the mailbox FILE (PST-OPEN)."
  (close (pst-file-stream file)))

(defmacro with-pst-file ((file path) &body body)
  "Run BODY with FILE the mailbox at PATH open for reading (PST-OPEN), closed
however BODY ends."
  `(let ((,file (pst-open ,path)))
     (unwind-protect (progn ,@body)
       (pst-close ,file))))

;;; The node database: blocks, data trees and subnode trees

(defun pst-read-block (file bid)
  "The data of the block BID of the mailbox FILE ([MS-PST] 2.2.2.8), decoded
when it is a data block of a mailbox whose blocks are encoded: where the
blocks' B-tree says it is, its trailer checked (a PST-UNREADABLE when it is
of another block or another size), its signature and check (CRC) noted
when wrong. While a message is read, it is counted against the message's
budget (*PST-BUDGET*): a PST-UNREADABLE once that is spent."
  (destructuring-bind (bids ibs cbs) (pst-file-blocks file)
    (let ((index (pst-search bids (logandc2 bid 1))))
      (unless index (pst-fail "block ~x is not in the blocks' B-tree" bid))
      (let* ((ib (svref ibs index)) (cb (svref cbs index))
             (unicode (pst-unicode-p file))
             (trailer (if unicode 16 12))
             (size (* 64 (ceiling (+ cb trailer) 64))))
        (when (> cb (pst-block-room file)) (pst-fail "block ~x says it holds ~:d octets, more than a block may" bid cb))
        (let* ((octets (pst-read-at file ib size))
               (at (- size trailer))
               (own (octets-integer octets (+ at (if unicode 8 4)) (if unicode 8 4)))
               (crc (octets-integer octets (+ at (if unicode 4 8)) 4)))
          (unless (= cb (octets-integer octets at 2))
            (pst-fail "block ~x at offset ~:d holds ~:d octets, not the ~:d the B-tree says"
                      bid ib (octets-integer octets at 2) cb))
          (unless (= (logandc2 own 1) (logandc2 bid 1))
            (pst-fail "the block at offset ~:d is block ~x, not ~x" ib own bid))
          (unless (= crc (pst-crc octets :end cb))
            (pst-note file "the check (CRC) of block ~x does not match its data" bid))
          (unless (= (octets-integer octets (+ at 2) 2) (pst-signature ib bid))
            (pst-note file "the signature of block ~x does not match" bid))
          (when (and *pst-budget* (minusp (decf (car *pst-budget*) cb)))
            (pst-fail "the message holds more than the ~a it is read with, each part counted as often as it ~
                       is named"
                      (octets-words (or *pst-message-budget* *outlook-largest-file*))))
          (let ((data (subseq octets 0 cb)))
            (if (logtest bid 2)
                data
                (case (pst-file-crypt file)
                  (1 (pst-unpermute data))
                  (2 (pst-cyclic data (logand bid #xFFFFFFFF)))
                  (t data)))))))))

(defun pst-data-blocks (file bid &key (limit *outlook-largest-file*))
  "The data blocks of the data tree BID of the mailbox FILE ([MS-PST]
2.2.2.8.3.2), a list in order: one block, or those an XBLOCK lists, or
those the XBLOCKs an XXBLOCK lists list. NIL for BID 0. A PST-UNREADABLE
when the tree is not as an XBLOCK says, names a block twice, or holds more
than LIMIT octets."
  (let ((seen (make-hash-table)) (total 0) (word (pst-word file)))
    (labels ((walk (bid level)
               (when (gethash bid seen) (pst-fail "the data tree names block ~x twice" bid))
               (setf (gethash bid seen) t)
               (let ((data (pst-read-block file bid)))
                 (cond ((not (logtest bid 2))
                        (when (and level (plusp level))
                          (pst-fail "the data tree's block ~x holds data where a list of blocks should be" bid))
                        (when (> (incf total (length data)) limit)
                          (pst-fail "it holds more than ~a (*outlook-largest-file*)" (octets-words limit)))
                        (list data))
                       (t (let ((type (pst-number-at data 0 1)) (own (pst-number-at data 1 1))
                                (count (pst-number-at data 2 2)) (whole (pst-number-at data 4 4)))
                            (unless (and (= type 1) (member own '(1 2)) (or (null level) (= own level)))
                              (pst-fail "block ~x is not the list of data blocks expected" bid))
                            (when (> whole limit)
                              (pst-fail "it holds ~a, more than ~a (*outlook-largest-file*)"
                                        (octets-words whole) (octets-words limit)))
                            (when (> (+ 8 (* count word)) (length data))
                              (pst-fail "block ~x lists more blocks than it holds" bid))
                            (loop for i below count
                                  append (walk (octets-integer data (+ 8 (* i word)) word) (1- own)))))))))
      (if (zerop bid) '() (walk bid nil)))))

(defun pst-data-octets (file bid &key (limit *outlook-largest-file*))
  "The octets of the data tree BID of the mailbox FILE, its blocks joined
\(PST-DATA-BLOCKS); an empty vector for BID 0."
  (let ((blocks (pst-data-blocks file bid :limit limit)))
    (if (and blocks (null (rest blocks))) (first blocks) (pst-join blocks))))

(defun pst-subnodes (file bid)
  "The nodes within a node of the mailbox FILE, its subnode tree BID
\([MS-PST] 2.2.2.8.3.3): a table from each one's NID to (DATA . SUB), the
BIDs of its data tree and its own subnode tree. An empty table for BID 0. A
PST-UNREADABLE when a block of the tree is not an SLBLOCK or SIBLOCK or is
named twice."
  (let ((table (make-hash-table)) (seen (make-hash-table)) (word (pst-word file))
        (head (if (pst-unicode-p file) 8 4)))
    (labels ((walk (bid level)
               (when (gethash bid seen) (pst-fail "the tree of nodes within a node names block ~x twice" bid))
               (setf (gethash bid seen) t)
               (let* ((data (pst-read-block file bid))
                      (type (pst-number-at data 0 1)) (own (pst-number-at data 1 1)) (count (pst-number-at data 2 2)))
                 (unless (and (= type 2) (member own '(0 1)) (or (null level) (= own level)))
                   (pst-fail "block ~x is not the list of nodes within a node expected" bid))
                 (let ((width (if (zerop own) (* 3 word) (* 2 word))))
                   (when (> (+ head (* count width)) (length data))
                     (pst-fail "block ~x lists more nodes than it holds" bid))
                   (dotimes (i count)
                     (let* ((at (+ head (* i width)))
                            (nid (logand #xFFFFFFFF (octets-integer data at word))))
                       (if (zerop own)
                           (setf (gethash nid table) (cons (octets-integer data (+ at word) word)
                                                           (octets-integer data (+ at word word) word)))
                           (walk (octets-integer data (+ at word) word) 0))))))))
      (unless (zerop bid) (walk bid nil))
      table)))

;;; Lists, tables and properties: the heap and its B-trees

(define-record (pst-hn (:constructor %make-pst-hn (blocks signature root)) (:copier nil))
  "A heap on a node ([MS-PST] 2.3.1) read: BLOCKS, a vector of its data
blocks; SIGNATURE, what it holds (#xBC a property context, #x7C a table
context, #xB5 a B-tree); ROOT, the heap identifier (HID) of its user root;
MAPS, each block's offsets of its items once read (HNPAGEMAP), NIL before."
  (blocks nil :documentation "The data blocks of PST-HN, a vector.")
  (signature nil :documentation "What PST-HN holds: #xBC a property context, #x7C a table context, #xB5 a B-tree.")
  (root nil :documentation "The heap identifier (HID) of the user root of PST-HN.")
  (maps (make-array 0) :documentation "Each block's offsets of the items of PST-HN once read (HNPAGEMAP), NIL before."))

(defun pst-heap-on (blocks)
  "The heap on BLOCKS, a node's data blocks (PST-DATA-BLOCKS), as a PST-HN;
a PST-UNREADABLE when its first block is no heap's."
  (let ((first (first blocks)))
    (unless (and first (>= (length first) 12) (= #xEC (aref first 2)))
      (pst-fail "the node's data is not a heap"))
    (let ((hn (%make-pst-hn (coerce blocks 'simple-vector) (aref first 3) (octets-integer first 4 4))))
      (setf (pst-hn-maps hn) (make-array (length blocks) :initial-element nil))
      hn)))

(defun pst-heap-map (hn index)
  "The offsets of the items of the block INDEX of the heap HN, a vector of
their count and one more (HNPAGEMAP's rgibAlloc), checked to lie in order
within the block."
  (or (svref (pst-hn-maps hn) index)
      (let* ((block (svref (pst-hn-blocks hn) index))
             (at (pst-number-at block 0 2))
             (count (pst-number-at block at 2))
             (offsets (make-array (1+ count))))
        (when (> (+ at 4 (* 2 (1+ count))) (length block))
          (pst-fail "the map of the heap's block ~d lies past its end" index))
        (dotimes (i (1+ count))
          (setf (svref offsets i) (octets-integer block (+ at 4 (* 2 i)) 2)))
        (loop for i from 1 to count
              unless (<= (svref offsets (1- i)) (svref offsets i) at)
                do (pst-fail "the map of the heap's block ~d is out of order" index))
        (setf (svref (pst-hn-maps hn) index) offsets))))

(defun pst-heap-item (hn hid)
  "The octets of the item HID of the heap HN ([MS-PST] 2.3.1.1), a new
vector; a PST-UNREADABLE for an identifier naming no item."
  (let ((block (ash hid -16)) (index (ldb (byte 11 5) hid)))
    (unless (and (zerop (logand hid #x1F)) (< block (length (pst-hn-blocks hn))) (plusp index))
      (pst-fail "~x names no item of the heap" hid))
    (let ((offsets (pst-heap-map hn block)))
      (unless (< index (length offsets)) (pst-fail "~x names no item of the heap" hid))
      (subseq (svref (pst-hn-blocks hn) block) (svref offsets (1- index)) (svref offsets index)))))

(defun pst-bth-records (hn hid)
  "The records of the B-tree on the heap HN whose header is the item HID
\([MS-PST] 2.3.2), each (KEY . DATA), two octet vectors, in the order of
their keys; its index levels followed to +PST-MOST-HEAP-LEVELS+, each item
read once. NIL for an empty tree."
  (let* ((header (pst-heap-item hn hid))
         (seen (make-hash-table)))
    (unless (and (= (length header) 8) (= #xB5 (aref header 0)))
      (pst-fail "the heap's item ~x is not the head of a B-tree" hid))
    (let ((key (aref header 1)) (entry (aref header 2)) (levels (aref header 3)) (root (octets-integer header 4 4)))
      (unless (and (member key '(2 4 8 16)) (<= 1 entry 32))
        (pst-fail "a B-tree on the heap has keys of ~d octets and records of ~d" key entry))
      (when (> levels +pst-most-heap-levels+)
        (pst-fail "a B-tree on the heap has ~d index levels, more than ~d" levels +pst-most-heap-levels+))
      (labels ((walk (hid level)
                 (when (gethash hid seen) (pst-fail "a B-tree on the heap names its item ~x twice" hid))
                 (setf (gethash hid seen) t)
                 (let* ((data (pst-heap-item hn hid))
                        (width (if (zerop level) (+ key entry) (+ key 4))))
                   (unless (zerop (mod (length data) width))
                     (pst-note nil "a B-tree's item on the heap is not a whole number of records"))
                   (if (zerop level)
                       (loop for at from 0 to (- (length data) width) by width
                             collect (cons (subseq data at (+ at key)) (subseq data (+ at key) (+ at width))))
                       (loop for at from 0 to (- (length data) width) by width
                             append (walk (octets-integer data (+ at key) 4) (1- level)))))))
        (if (zerop root) '() (walk root levels))))))

;;; Lists, tables and properties: property and table contexts

(define-record (pst-node-data (:constructor make-pst-node-data (file hn subnodes)) (:copier nil))
  "What the values of a property or table context are read from: the
mailbox FILE, the heap HN on the node's data, and SUBNODES, the nodes within
the node (PST-SUBNODES)."
  (file nil :documentation "The mailbox the values of PST-NODE-DATA are read from.")
  (hn nil :documentation "The heap on the data of the node of PST-NODE-DATA.")
  (subnodes nil :documentation "The nodes within the node of PST-NODE-DATA (PST-SUBNODES)."))

(defun pst-hnid-value (data hnid)
  "The octets the HNID ([MS-PST] 2.3.3.2) names in DATA, a PST-NODE-DATA: an
item of the heap, or the data of a node within the node, held to
*OUTLOOK-LARGEST-FILE*; an empty vector for 0."
  (cond ((zerop hnid) (pst-octets 0))
        ((zerop (logand hnid #x1F)) (pst-heap-item (pst-node-data-hn data) hnid))
        (t (let ((node (gethash hnid (pst-node-data-subnodes data))))
             (unless node (pst-fail "the value kept in node ~x within the node is not there" hnid))
             (pst-data-octets (pst-node-data-file data) (car node))))))

(defun pst-object-nid (data hnid)
  "The NID of the node within the node holding an object (a message
attached), named by HNID in DATA, a PST-NODE-DATA: an item of the heap
holding the NID and the object's size ([MS-PST] 2.3.3.5), or the NID itself."
  (if (zerop (logand hnid #x1F))
      (pst-number-at (pst-heap-item (pst-node-data-hn data) hnid) 0 4)
      hnid))

(defun pst-node-heap (file data-bid sub-bid signature)
  "The heap of the node of the mailbox FILE whose data tree is DATA-BID and
subnode tree SUB-BID, as a PST-NODE-DATA, checked to hold what SIGNATURE says
\(#xBC a property context, #x7C a table context)."
  (let ((hn (pst-heap-on (pst-data-blocks file data-bid))))
    (unless (= signature (pst-hn-signature hn))
      (pst-fail "the node holds ~:[a table~;properties~], not ~:[a table~;properties~]"
                (= (pst-hn-signature hn) #xBC) (= signature #xBC)))
    (make-pst-node-data file hn (pst-subnodes file sub-bid))))

(defun pst-read-properties (file data-bid sub-bid)
  "The properties of the property context ([MS-PST] 2.3.3) on the node of
the mailbox FILE whose data tree is DATA-BID and subnode tree SUB-BID, each
\(TAG . VALUE): VALUE its octets as PST-MESSAGE keeps them, cut to its size
for a fixed size; for an object, the four octets of the NID of the node
within holding it. A value that cannot be read is noted and left out. The
node's PST-NODE-DATA is the second value."
  (let* ((data (pst-node-heap file data-bid sub-bid #xBC))
         (hn (pst-node-data-hn data))
         (records (pst-bth-records hn (pst-hn-root hn))))
    (values
     (loop for (key . entry) in records
           for id = (octets-integer key 0 2)
           for type = (and id (>= (length entry) 6) (octets-integer entry 0 2))
           for hnid = (and type (octets-integer entry 2 4))
           for size = (and type (pst-fixed-size type))
           for value = (and type
                            (handler-case
                                (cond ((and size (<= size 4)) (subseq entry 2 (+ 2 size)))
                                      ((= type #x000D) (pst-number (pst-object-nid data hnid) 4))
                                      (t (let ((octets (pst-hnid-value data hnid)))
                                           (if (and size (< (length octets) size))
                                               (pst-fail "a value of ~d octets where ~d are due" (length octets) size)
                                               (if size (subseq octets 0 size) octets)))))
                              (pst-unreadable (condition)
                                (pst-note file "property ~4,'0x~4,'0x could not be read: ~a" id type condition)
                                nil)))
           when value collect (cons (logior (ash id 16) type) value))
     data)))

(defun pst-read-table (file data-bid sub-bid)
  "The rows of the table context ([MS-PST] 2.3.4) on the node of the mailbox
FILE whose data tree is DATA-BID and subnode tree SUB-BID, in the order of
the table's index of rows, each (ROW-ID . PROPERTIES), PROPERTIES each (TAG .
VALUE) as PST-READ-PROPERTIES gives them, those of its own (the row's
identifier and version) left out. A cell that cannot be read is noted and
left out."
  (let* ((data (pst-node-heap file data-bid sub-bid #x7C))
         (hn (pst-node-data-hn data))
         (info (pst-heap-item hn (pst-hn-root hn))))
    (unless (and (>= (length info) 22) (= #x7C (aref info 0)))
      (pst-fail "the table's description is not one"))
    (let* ((count (aref info 1))
           (bitmap (octets-integer info 6 2))
           (size (octets-integer info 8 2))
           (columns (progn (when (> (+ 22 (* 8 count)) (length info)) (pst-fail "the table's columns lie past its end"))
                           (loop for i below count for at = (+ 22 (* 8 i))
                                 collect (list (octets-integer info at 4) (octets-integer info (+ at 4) 2)
                                               (aref info (+ at 6)) (aref info (+ at 7))))))
           (rows-hnid (octets-integer info 14 4))
           (blocks (coerce (cond ((or (zerop rows-hnid) (zerop size)) '())
                                 ((zerop (logand rows-hnid #x1F)) (list (pst-heap-item hn rows-hnid)))
                                 (t (let ((node (gethash rows-hnid (pst-node-data-subnodes data))))
                                      (unless node (pst-fail "the table's rows are not in the node"))
                                      (pst-data-blocks file (car node)))))
                           'simple-vector))
           (per-block (if (plusp size) (floor (pst-block-room file) size) 0)))
      (when (> bitmap size) (pst-fail "the table's rows are ~d octets, shorter than their parts" size))
      (loop for (tag offset width) in columns
            unless (<= (+ offset width) bitmap)
              do (pst-fail "column ~8,'0x of the table lies past the values of its rows" tag))
      (flet ((row (index)
               (let* ((one (<= (length blocks) 1))
                      (block (if one 0 (floor index (max 1 per-block))))
                      (at (* size (if one index (mod index (max 1 per-block)))))
                      (octets (and (< block (length blocks)) (svref blocks block))))
                 (unless (and octets (<= (+ at size) (length octets)))
                   (pst-fail "row ~d of the table lies past its rows" index))
                 (values octets at))))
        (loop for (key . value) in (pst-bth-records hn (octets-integer info 10 4))
              for row-id = (octets-integer key 0 (length key))
              for index = (octets-integer value 0 (length value))
              collect (cons row-id
                            (handler-case
                                (multiple-value-bind (octets base) (row index)
                                  (pst-row-cells data octets base columns bitmap))
                              (pst-unreadable (condition)
                                (pst-note file "a row of a table could not be read: ~a" condition)
                                '()))))))))

(defun pst-row-cells (data octets base columns bitmap)
  "The cells of a row of a table context, its octets in OCTETS from BASE,
each (TAG . VALUE) as PST-READ-PROPERTIES gives values: those COLUMNS, each
\(TAG OFFSET WIDTH BIT), say are there in the bits from BITMAP on, but the
row's own identifier and version. A value of eight octets or fewer is in the
row, any other is named by an HNID there, four octets, and read from DATA, a
PST-NODE-DATA; one that cannot be read is noted and left out."
  (loop for (tag offset width bit) in columns
        for fixed = (pst-fixed-size (pst-property-type tag))
        for at = (+ base bitmap (floor bit 8))
        when (and (not (member tag '(#x67F20003 #x67F30003)))
                  (< at (length octets))
                  (logbitp (- 7 (mod bit 8)) (aref octets at)))
          append (handler-case
                     (let ((cell (subseq octets (+ base offset) (+ base offset width))))
                       (list (cons tag (cond ((and fixed (<= fixed 8))
                                              (if (>= width fixed)
                                                  (subseq cell 0 fixed)
                                                  (pst-fail "a cell of ~d octets holds a value of ~d" width fixed)))
                                             ((< width 4)
                                              (pst-fail "a cell of ~d octets holds no reference to its value" width))
                                             (t (pst-hnid-value data (octets-integer cell 0 4)))))))
                   (pst-unreadable (condition)
                     (pst-note (pst-node-data-file data) "a cell of a table could not be read: ~a" condition)
                     '()))))

;;; Messaging: named properties and the message store

(defun pst-names (file)
  "The map of named properties of the mailbox FILE ([MS-PST] 2.4.7), read
once, as the mailbox is opened (PST-OPEN), under a budget of its own, never
a message's: a table from each property number from #x8000 to (GUID . NAME)
\(PST-NAME-TABLE). Empty when the map cannot be read, noted among the
mailbox's problems and kept as its NAMES-FAILURE."
  (or (pst-file-names file)
      (setf (pst-file-names file)
            (handler-case
                (let ((*pst-budget* (cons *outlook-largest-file* 0)))
                  (multiple-value-bind (data sub) (pst-find-node file #x61)
                    (if data
                        (let ((properties (pst-read-properties file data sub)))
                          (pst-name-table (pst-value properties #x00020102) (pst-value properties #x00030102)
                                          (pst-value properties #x00040102)))
                        (make-hash-table))))
              (pst-unreadable (condition)
                (setf (pst-file-names-failure file) (princ-to-string condition))
                (pst-problem file (format nil "the map of named properties could not be read: ~a" condition))
                (make-hash-table))))))

(defun pst-entry-nid (entry-id)
  "The node number an entry identifier ([MS-PST] 2.4.3.2), ENTRY-ID's octets,
names: its last four octets; NIL when it is too short to be one."
  (and entry-id (= (length entry-id) 24) (octets-integer entry-id 20 4)))

(defun pst-store (file)
  "What the message store of the mailbox FILE says of it ([MS-PST] 2.4.3):
its name, its record key, and the NIDs of its top folder (the root of the
personal folders, IPM subtree) and of its deleted items, four values; the
standard NIDs where it does not say, and no name or key when it cannot be
read."
  (handler-case
      (multiple-value-bind (data sub) (pst-find-node file #x21)
        (unless data (pst-fail "it has no message store"))
        (let ((properties (pst-read-properties file data sub)))
          (values (let ((name (or (pst-value properties #x3001001F) (pst-value properties #x3001001E))))
                    (and name (plusp (length name)) (pst-string-text #x3001001F name)))
                  (pst-value properties #x0FF90102)
                  (or (pst-entry-nid (pst-value properties #x35E00102)) #x8022)
                  (or (pst-entry-nid (pst-value properties #x35E30102)) #x8062))))
    (pst-unreadable (condition)
      (pst-problem file (format nil "the message store could not be read: ~a" condition))
      (values nil nil #x8022 #x8062))))

(defun pst-string-text (tag octets &optional charset)
  "The text of the string property TAG whose value is OCTETS: UTF-16 for a
Unicode string, else eight-bit characters in CHARSET (windows-1252 when NIL),
the nulls after either left out."
  (string-right-trim (string (code-char 0))
                     (if (= #x001F (pst-property-type tag))
                         (pst-utf-16-text octets)
                         (charset-text octets (or charset "windows-1252")))))

;;; Messaging: folders

(define-record (pst-read-folder (:constructor make-pst-read-folder (nid parent name)) (:copier nil))
  "A folder of a mailbox read: its NID, its PARENT's NID and its NAME (NIL
when its properties could not be read). CHILDREN is its folders' NIDs and
MESSAGES its messages', each a list in order."
  (nid nil :documentation "The node number (NID) of PST-READ-FOLDER.")
  (parent nil :documentation "The NID of the parent of PST-READ-FOLDER.")
  (name nil :documentation "The name of PST-READ-FOLDER, NIL when its properties could not be read.")
  (children '() :documentation "The NIDs of the folders in PST-READ-FOLDER, a list in order.")
  (messages '() :documentation "The NIDs of the messages in PST-READ-FOLDER, a list in order."))

(defun pst-folder-display-name (file nid data-bid sub-bid)
  "The name of the folder NID of the mailbox FILE whose properties' node has
the data tree DATA-BID and subnode tree SUB-BID: its display name, \"\" for
none; NIL, noted, when its properties cannot be read."
  (handler-case
      (let* ((properties (pst-read-properties file data-bid sub-bid))
             (tag (find-if (lambda (tag) (assoc tag properties)) '(#x3001001F #x3001001E))))
        (if tag (pst-string-text tag (pst-value properties tag)) ""))
    (pst-unreadable (condition)
      (pst-problem file (format nil "folder ~x could not be read: ~a" nid condition))
      nil)))

(defun pst-folder-table (file)
  "The folders of the mailbox FILE, a table from each NID to its
PST-READ-FOLDER: every normal folder and search folder in the nodes'
B-tree, named by its properties, with its folders and its messages, those
in the order of their NIDs, as the nodes' B-tree links each to its parent."
  (let ((table (make-hash-table)))
    (destructuring-bind (nids datas subs parents) (pst-file-nodes file)
      (dotimes (i (length nids))
        (let ((nid (svref nids i)))
          (when (member (logand nid #x1F) '(2 3))
            (setf (gethash nid table)
                  (make-pst-read-folder nid (svref parents i)
                                        (pst-folder-display-name file nid (svref datas i) (svref subs i)))))))
      (loop for i from (1- (length nids)) downto 0
            for nid = (svref nids i)
            for parent = (gethash (svref parents i) table)
            do (case (logand nid #x1F)
                 ((2 3) (when (and parent (/= nid (svref parents i))) (push nid (pst-read-folder-children parent))))
                 (4 (when parent (push nid (pst-read-folder-messages parent)))))))
    table))

(defun pst-message-nids (file)
  "The NIDs of every normal message of the mailbox FILE, in order, a list."
  (loop for nid across (first (pst-file-nodes file)) when (= 4 (logand nid #x1F)) collect nid))

;;; Messaging: messages

(defun pst-names-used (file lists)
  "What each named property the property LISTS hold stands for, as
PST-MESSAGE's NAMES keeps it, from the map of the mailbox FILE (PST-NAMES).
When the map could not be read, the message's notes say which of them
stand for nothing known."
  (let ((names (pst-names file)) (used '()) (unknown '()))
    (dolist (properties lists)
      (loop for (tag) in properties
            for id = (ldb (byte 16 16) tag)
            for name = (and (>= id #x8000) (gethash id names))
            do (cond ((and name (not (assoc id used))) (push (cons id name) used))
                     ((and (>= id #x8000) (not name)) (pushnew id unknown)))))
    (when (and unknown (pst-file-names-failure file))
      (pst-note nil "named properties ~{~4,'0x~^, ~} stand for nothing known: the map of named properties ~
                     could not be read (~a)"
                (sort unknown #'<) (pst-file-names-failure file)))
    (nreverse used)))

(defun pst-attachment-in (file node depth seen)
  "The properties of the attachment whose node within its message is NODE,
\(DATA . SUB), in the mailbox FILE, the message DEPTH deep: a message
attached ([MS-PST] 2.4.6.2) read as one while DEPTH is below
*OUTLOOK-NESTING-LIMIT*, none of SEEN, the nodes of those it is within, and
the message's budget allows another (*PST-BUDGET*); an embedded object's
octets, or an object whose attachment says not how it is attached, as its
contents."
  (multiple-value-bind (properties data) (pst-read-properties file (car node) (cdr node))
    (let ((object (assoc #x3701000D properties)))
      (when object
        (let* ((nid (octets-integer (cdr object) 0 4))
               (inner (gethash nid (pst-node-data-subnodes data)))
               (method (let ((value (pst-value properties #x37050003))) (and value (octets-integer value 0 4)))))
          (setf properties (remove object properties))
          (cond ((null inner) (pst-note file "the object attached, node ~x, is not in the attachment" nid))
                ((not (eql method 5))
                 (push (cons #x37010102 (pst-data-octets file (car inner))) properties))
                ((or (>= depth *outlook-nesting-limit*) (member (car inner) seen))
                 (pst-note file "a message attached ~d deep was not read (*outlook-nesting-limit*)" (1+ depth)))
                ((and *pst-budget* (minusp (decf (cdr *pst-budget*))))
                 (pst-note file "a message attached was not read: the message holds more than ~d"
                           +pst-most-attached-messages+))
                (t (push (cons #x3701000D (pst-message-in file (car inner) (cdr inner) (1+ depth)
                                                          (cons (car inner) seen)))
                         properties)))))
      properties)))

(defun pst-message-in (file data-bid sub-bid depth seen)
  "The message on the node of the mailbox FILE whose data tree is DATA-BID
and subnode tree SUB-BID, DEPTH deep, as a PST-MESSAGE: its properties, its
recipients' (its recipient table's rows), its attachments' (each in the
order of its attachment table, then any it holds that the table does not
list), each message attached read as one (PST-ATTACHMENT-IN), and what its
named properties stand for. A part that cannot be read is noted; an
attachment that cannot be read is one whose name says so."
  (multiple-value-bind (properties data) (pst-read-properties file data-bid sub-bid)
    (let* ((subnodes (pst-node-data-subnodes data))
           (recipients (let ((table (gethash #x692 subnodes)))
                         (and table (handler-case (mapcar #'cdr (pst-read-table file (car table) (cdr table)))
                                      (pst-unreadable (condition)
                                        (pst-note file "its recipients could not be read: ~a" condition)
                                        '())))))
           (listed (let ((table (gethash #x671 subnodes)))
                     (and table (handler-case (mapcar #'car (pst-read-table file (car table) (cdr table)))
                                  (pst-unreadable (condition)
                                    (pst-note file "its table of attachments could not be read: ~a" condition)
                                    '())))))
           (unlisted (sort (loop for nid being the hash-keys of subnodes
                                 when (and (= 5 (logand nid #x1F)) (not (member nid listed))) collect nid)
                           #'<))
           (attachments
             (loop for nid in (append listed unlisted)
                   for node = (gethash nid subnodes)
                   collect (handler-case
                               (progn (unless node (pst-fail "it is listed but not in the message"))
                                      (when (member nid unlisted)
                                        (pst-note file "attachment ~x is not listed in its message's table" nid))
                                      (pst-attachment-in file node depth seen))
                             (pst-unreadable (condition)
                               (pst-note file "attachment ~x could not be read: ~a" nid condition)
                               (list (pst-property #x3707001F (format nil "attachment ~x, which could not be read"
                                                                      nid))))))))
      (make-pst-message :properties properties :recipients recipients :attachments attachments
                        :names (pst-names-used file (append (list properties) recipients attachments))))))

(defun pst-read-message (file nid)
  "The message NID of the mailbox FILE, a PST-MESSAGE (PST-MESSAGE-IN); a
PST-UNREADABLE saying why when it cannot be read at all. It reads no more
than *PST-MESSAGE-BUDGET* octets of blocks, a block named twice counted
twice, and no more than +PST-MOST-ATTACHED-MESSAGES+ messages attached, its
attachments and theirs together (*PST-BUDGET*): what is past them is listed
as not read."
  (multiple-value-bind (data sub) (pst-find-node file nid)
    (unless data (pst-fail "message ~x is not in the nodes' B-tree" nid))
    (let ((*pst-budget* (cons (or *pst-message-budget* *outlook-largest-file*) +pst-most-attached-messages+)))
      (pst-message-in file data sub 0 (list data)))))

;;; Messaging: the folders listed

(defconstant +pst-most-folder-depth+ 64
  "The deepest folder of a mailbox listed in its place; one deeper is listed
among the orphans.")

(defun pst-path-name (name)
  "The folder NAME as a step of a path: / made -, nulls left out, . and ..
and none given named in words, cut to 240 octets (FIT-NAME), room left for a
number after it."
  (let ((name (fit-name (substitute #\- #\/ (remove (code-char 0) (or name ""))) :most 240)))
    (cond ((string= name "") "(no name)")
          ((string= name ".") "(.)")
          ((string= name "..") "(..)")
          (t name))))

(defconstant +pst-most-path-octets+ 1024
  "The most octets in UTF-8 a folder's path from the directory a mailbox is
written in should take: a name that would take it past them is cut to fit,
though to no fewer than 16 octets.")

(defparameter *mailbox-path-file* ".path"
  "The file in the directory of a folder written of a mailbox that holds the
folder's path whole, its names joined by /, when the directory's path was
cut to fit (PST-LISTED-FOLDERS): the list shows it in place of the path.")

(defun pst-written-name-p (key)
  "True when KEY, a name in lower case, is one the reader writes a file by
beside the folders of a folder: *MAILBOX-PATH-FILE*, or a message's file, a
number with .eml, .msg, .unreadable, .vcf or .ics after it."
  (let ((dot (position #\. key)))
    (or (string= key *mailbox-path-file*)
        (and dot (plusp dot) (every #'digit-char-p (subseq key 0 dot))
             (member (subseq key (1+ dot)) '("eml" "msg" "unreadable" "vcf" "ics") :test #'string=)))))

(defun pst-folder-step (above name used)
  "NAME, a folder's name as a step of a path (PST-PATH-NAME), as its directory
is named below ABOVE, the steps above it: cut, when it would make the path
longer than +PST-MOST-PATH-OCTETS+ octets, to the room left but to no fewer
than 16 octets (FIT-NAME); then told apart from every name in USED, those of
the folders beside it, compared without case, and from every name the reader
writes a file by there (PST-WRITTEN-NAME-P), by (2), (3) after it, and
noted there. Folders whose names differ only in case so stay apart on a file
system that ignores case. The step is the first value; the second is the
name as the list shows it: the step, or when it was cut, NAME whole with the
same number after it."
  (let* ((taken (reduce #'+ above :key (lambda (step) (1+ (length (string-octets step))))))
         (base (fit-name name :most (max 16 (- +pst-most-path-octets+ taken 12)))))
    (loop for count from 1
          for step = (if (= count 1) base (format nil "~a (~d)" base count))
          for key = (string-downcase step)
          unless (or (gethash key used) (pst-written-name-p key))
            do (setf (gethash key used) t)
               (return (values step (cond ((eq base name) step)
                                          ((= count 1) name)
                                          (t (format nil "~a (~d)" name count))))))))

(defun pst-listed-folders (file &key (deleted t))
  "The folders of the mailbox FILE that hold messages, each (PATH . NIDS):
PATH a list of names, NIDS its messages' in order. The top of the personal
folders first, named by the message store (as readpst named it), and the
folders within it, in the order of their NIDs, those of deleted items left
out unless DELETED; then the other folders of the root, under Outside the
top folder; then the messages whose folder is not reached from the root, or
lies more than +PST-MOST-FOLDER-DEPTH+ deep, under Orphans, in a folder of
their folder's name. Each name is made a step of a path as PST-FOLDER-STEP
says: two folders in one folder whose names are alike but for case are told
apart by (2), (3) after the name, and a name that would make the path too
long is cut. The second value lists each folder whose path was cut so, as
\(PATH . NAMES), NAMES its names whole."
  (let* ((folders (pst-folder-table file))
         (listed '()) (cut '()) (reached (make-hash-table)) (roots (make-hash-table :test 'equal)))
    (multiple-value-bind (store-name record-key top waste) (pst-store file)
      (declare (ignore record-key))
      (labels ((folder-name (nid) (let ((folder (gethash nid folders)))
                                    (pst-path-name (and folder (pst-read-folder-name folder)))))
               (list-folder (path names nids)
                 (push (cons path nids) listed)
                 (unless (equal path names) (push (cons path names) cut)))
               (walk (nid path names depth listing)
                 (let ((folder (gethash nid folders))
                       (listing (and listing (or deleted (/= nid waste)))))
                   (when (and folder (not (gethash nid reached)) (<= depth +pst-most-folder-depth+))
                     (setf (gethash nid reached) t)
                     (when (and listing (pst-read-folder-messages folder))
                       (list-folder path names (pst-read-folder-messages folder)))
                     (walk-children folder path names depth listing))))
               (walk-children (folder path names depth listing)
                 (let ((used (make-hash-table :test 'equal)))
                   (dolist (child (sort (copy-list (pst-read-folder-children folder)) #'<))
                     (multiple-value-bind (step shown) (pst-folder-step path (folder-name child) used)
                       (walk child (append path (list step)) (append names (list shown)) (1+ depth) listing))))))
        (multiple-value-bind (step shown)
            (pst-folder-step '() (pst-path-name (or store-name (let ((folder (gethash top folders)))
                                                                 (and folder (pst-read-folder-name folder)))))
                             roots)
          (walk top (list step) (list shown) 1 t))
        (setf (gethash #x122 reached) t)
        (let ((root (gethash #x122 folders))
              (outside (pst-folder-step '() "Outside the top folder" roots)))
          (when root
            (when (pst-read-folder-messages root)
              (list-folder (list outside) (list outside) (pst-read-folder-messages root)))
            (walk-children root (list outside) (list outside) 1 t)))
        (let ((orphans (make-hash-table :test 'equal))
              (step (pst-folder-step '() "Orphans" roots))
              (used (make-hash-table :test 'equal)))
          (dolist (nid (pst-message-nids file))
            (multiple-value-bind (data sub parent) (pst-find-node file nid)
              (declare (ignore data sub))
              (unless (gethash parent reached)
                (push nid (gethash (if (gethash parent folders)
                                       (folder-name parent)
                                       (format nil "folder ~(~x~), not in the mailbox" parent))
                                   orphans)))))
          (loop for name in (sort (loop for name being the hash-keys of orphans collect name) #'string<)
                do (multiple-value-bind (inner shown) (pst-folder-step (list step) name used)
                     (list-folder (list step inner) (list step shown) (reverse (gethash name orphans))))))
        (values (nreverse listed) (nreverse cut))))))
