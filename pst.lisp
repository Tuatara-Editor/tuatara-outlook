;;;; pst.lisp --- Outlook mailboxes (.pst) written: a new file of folders and their messages.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mailbox is written as Microsoft's [MS-PST] lays out the Unicode format
;;;; (revision 11.2, February 2025; section numbers below are of it), in its
;;;; three layers. The node database (NDB, 2.2) keeps nodes, each a number
;;;; (NID) naming its data and the nodes within it, in blocks of at most
;;;; 8 KB found through two B-trees of 512-octet pages, and allocation maps
;;;; saying which 64-octet slots of the file are taken. The lists, tables and
;;;; properties (LTP, 2.3) keep a heap in a node's data, and on it a B-tree
;;;; of properties (a property context) or a table of rows (a table context).
;;;; The messaging layer (2.4, 2.7) makes of those the message store, the
;;;; folders every mailbox has, and the folders and messages written.
;;;;
;;;; The file is written front to back as the messages come, a block at a
;;;; time, only the B-trees' entries and the folders' rows kept meanwhile; the
;;;; B-trees, the allocation maps and the header last. Strings are written in
;;;; UTF-16; the data blocks permuted (NDB_CRYPT_PERMUTE, as Outlook writes
;;;; them), cyclic (NDB_CRYPT_CYCLIC) or as they are. A file is at most 50 GB,
;;;; the most Outlook takes. The ANSI format of Outlook 97 to 2002, its
;;;; numbers and offsets of four octets, is written too, for the tests of the
;;;; reader (pst-read.lisp).

(in-package #:outlook)

;;; The node database: where things go in the file

(defconstant +pst-first-amap+ #x4400
  "Where the first allocation map (AMap) page is, [MS-PST] 2.2.2.7.2.")

(defconstant +pst-amap-span+ 253952
  "The octets each AMap maps, its own page among them: 496 octets of bits, each
for 64 octets ([MS-PST] 2.2.2.7.2).")

(defconstant +pst-largest-file+ (* 50 1024 1024 1024)
  "The largest mailbox written, 50 GB: the most Outlook lets a Unicode mailbox
grow to as it is installed (its MaxLargeFileSize). The format itself goes
further, to about 266 GB, where the density list can no longer number the
AMaps ([MS-PST] 2.2.2.7.4.1).")

(defun pst-section-start (section)
  "Where the part of the file the AMap numbered SECTION (from 0) maps begins:
at that AMap."
  (+ +pst-first-amap+ (* section +pst-amap-span+)))

(defun pst-fmap-section-p (section)
  "True when the part of the file SECTION's AMap maps holds a free map (FMap,
[MS-PST] 2.2.2.7.5), its third page: each 496th from the 128th, the first
128 being mapped by the header's own (1.3.2.7)."
  (and (>= section 128) (zerop (mod (- section 128) 496))))

(defun pst-fpmap-section-p (section)
  "True when the part of the file SECTION's AMap maps holds a free page map
\(FPMap, [MS-PST] 2.2.2.7.6), its third page: each 31,744th (an FPMap's 3,968
bits, one for each PMap of eight AMaps) from the 8,192nd, the first 1,024
PMaps, about 2 GB, being mapped by the header's own (1.3.2.8). An FMap and an
FPMap never fall in the same part."
  (and (>= section 8192) (zerop (mod (- section 8192) 31744))))

(defun pst-section-maps (section)
  "How many map pages begin the part of the file SECTION's AMap maps: the AMap;
a page map (PMap) in every eighth ([MS-PST] 2.2.2.7.3); then a free map
\(FMap) or a free page map (FPMap) where one is due."
  (+ 1 (if (zerop (mod section 8)) 1 0)
     (if (or (pst-fmap-section-p section) (pst-fpmap-section-p section)) 1 0)))

(defun pst-section-data (section)
  "Where data may begin in the part of the file SECTION's AMap maps, past its
map pages."
  (+ (pst-section-start section) (* 512 (pst-section-maps section))))

(defun pst-section-of (at)
  "The number of the AMap mapping the file offset AT."
  (floor (- at +pst-first-amap+) +pst-amap-span+))

(define-record (pst-writer (:constructor make-pst-writer (fd path &key (encoding :permute) (format :unicode)))
                       (:copier nil))
  "A mailbox being written to the file open as file descriptor (FD) at PATH,
its data blocks ENCODING, in FORMAT. END is the first octet not yet given out
and FREE the stretches before it given to nothing. BLOCKS and NODES hold what
the two B-trees will be made from, and NAMES numbers the named properties of
the messages written."
  (fd nil :documentation "The file descriptor (FD) the mailbox is written to.")
  (path nil :documentation "The path of the file PST-WRITER writes.")
  (encoding nil :documentation "How the data blocks are encoded, :permute, :cyclic or :none.")
  (format nil :documentation "The format written, :unicode, or :ansi (Outlook 97 to 2002's).")
  (end (pst-section-data 0) :documentation "The first octet of the file not yet given out.")
  (free '() :documentation "The stretches before the end given to nothing, each (START . END), latest first.")
  (next-block 1 :documentation "The counter of the next block's identifier.")
  (next-page 2 :documentation "The next page's identifier, even.")
  (names (let ((names (make-hash-table :test 'equalp)))
           (setf (gethash (cons *pst-common-guid* #x8501) names) #x8000
                 (gethash (cons *pst-public-strings-guid* "Keywords") names) #x8001)
           names)
         :documentation "A table numbering the named properties of the messages written, from (GUID . NAME) to a number
from #x8000 (PST-RENAMED). Two of Outlook's own names are in it from the start, as libpff refuses a map with an empty
stream.")
  (blocks (make-array 1024 :adjustable t :fill-pointer 0)
          :documentation "Three numbers for each block written, in the order of their identifiers: its identifier, where
it is, how many octets of data.")
  (nodes (make-array 256 :adjustable t :fill-pointer 0)
         :documentation "The entry of each node in the node B-tree, (NID DATA SUB PARENT).")
  (last-nids (let ((last (make-array 32 :initial-element #x400)))
               (setf (aref last 2) #x403 (aref last #xD) #x403 (aref last #xE) #x403 (aref last #xF) #x403
                     (aref last 3) #x4000 (aref last 4) #x10000 (aref last 8) #x8000)
               last)
             :documentation "The last index given to each type of node number (the header's rgnid)."))

(defun pst-writer-ansi-p (writer)
  "True when WRITER writes the ANSI format, else the Unicode one."
  (eq (pst-writer-format writer) :ansi))

(defun pst-writer-word (writer)
  "The octets of a block identifier or an offset in the mailbox WRITER
writes: eight in the Unicode format, four in the ANSI one."
  (if (pst-writer-ansi-p writer) 4 8))

(defun pst-writer-room (writer)
  "The most octets of data a block of the mailbox WRITER writes holds: 8,176
in the Unicode format, 8,180 in the ANSI one ([MS-PST] 2.2.2.8)."
  (if (pst-writer-ansi-p writer) 8180 8176))

(defun pst-note-free (writer from to)
  "Note that the octets of the file from FROM below TO are given to nothing,
the map pages among them excepted."
  (loop for section from (pst-section-of from) to (pst-section-of (1- to))
        for start = (max from (pst-section-data section))
        for end = (min to (pst-section-start (1+ section)))
        when (< start end) do (push (cons start end) (pst-writer-free writer))))

(defun pst-allocate (writer size align)
  "Give out SIZE octets of the file, aligned on ALIGN, after all given before
and never across a part of the file another AMap maps nor over its map pages
([MS-PST] 2.6.1.1.1); return where they begin. An error past
+PST-LARGEST-FILE+."
  (let* ((end (pst-writer-end writer))
         (at (* align (ceiling end align))))
    (loop for section = (pst-section-of at)
          do (cond ((< at (pst-section-data section)) (setf at (pst-section-data section)))
                   ((> (+ at size) (pst-section-start (1+ section))) (setf at (pst-section-data (1+ section))))
                   (t (return))))
    (when (> (+ at size) +pst-largest-file+)
      (error "The mailbox would grow past ~d GB, the most Outlook takes" (floor +pst-largest-file+ (expt 1024 3))))
    (when (> at end) (pst-note-free writer end at))
    (setf (pst-writer-end writer) (+ at size))
    at))

(defun pst-write-at (writer octets at)
  "Write OCTETS into the mailbox's file at the offset AT."
  (write-octets (pst-writer-fd writer) octets :at at :path (pst-writer-path writer)))

(defun pst-new-nid (writer type)
  "A new node number (NID, [MS-PST] 2.2.2.1) of TYPE for a node of the whole
mailbox: the next index for TYPE, counted in the header."
  (let ((index (incf (aref (pst-writer-last-nids writer) type))))
    (logior (ash index 5) type)))

(defun pst-folder-nid (index type)
  "The node number of the part TYPE of the folder whose index is INDEX: its
properties (2), hierarchy (#xD), contents (#xE) or associated contents
\(#xF) table ([MS-PST] 2.4.4)."
  (logior (ash index 5) type))

;;; The node database: blocks and nodes

(defun pst-block (writer data &key internal)
  "Write DATA, at most PST-WRITER-ROOM octets, as a new block ([MS-PST]
2.2.2.8): padded to a multiple of 64 octets with its trailer, a data block
encoded as the mailbox is; INTERNAL for a block of the node database's own
\(a data tree's or a subnode tree's). Returns its block identifier (BID)."
  (let ((cb (length data)) (ansi (pst-writer-ansi-p writer)))
    (assert (<= cb (pst-writer-room writer)))
    (let* ((trailer (if ansi 12 16))
           (size (* 64 (ceiling (+ cb trailer) 64)))
           (bid (logior (* 4 (pst-writer-next-block writer)) (if internal 2 0)))
           (at (pst-allocate writer size 64))
           (stored (let ((copy (copy-seq (coerce data 'pst-octets))))
                     (if internal
                         copy
                         (case (pst-writer-encoding writer)
                           (:permute (pst-permute copy))
                           (:cyclic (pst-cyclic copy (logand bid #xFFFFFFFF)))
                           (t copy)))))
           (block (pst-octets size))
           (end (- size trailer)))
      (incf (pst-writer-next-block writer))
      (replace block stored)
      (pst-put block end cb 2)
      (pst-put block (+ end 2) (pst-signature at bid) 2)
      (if ansi
          (progn (pst-put block (+ end 4) bid 4) (pst-put block (+ end 8) (pst-crc stored) 4))
          (progn (pst-put block (+ end 4) (pst-crc stored) 4) (pst-put block (+ end 8) bid 8)))
      (pst-write-at writer block at)
      (let ((blocks (pst-writer-blocks writer)))
        (vector-push-extend bid blocks)
        (vector-push-extend at blocks)
        (vector-push-extend cb blocks))
      bid)))

(defun pst-chunks (octets &key (size 8176))
  "OCTETS cut into pieces of SIZE octets, the last perhaps shorter, as the data
blocks of a data tree hold them."
  (loop for start from 0 below (length octets) by size
        collect (subseq octets start (min (length octets) (+ start size)))))

(defun pst-data-tree (writer chunks)
  "Write CHUNKS, a list of pieces of at most PST-WRITER-ROOM octets each, as
a data tree ([MS-PST] 2.2.2.8.3.2): one data block, or data blocks listed by
an XBLOCK, or XBLOCKs listed by an XXBLOCK. Returns its BID; 0 for no
chunks."
  (let* ((word (pst-writer-word writer))
         (per (floor (- (pst-writer-room writer) 8) word)))
    (labels ((listing (level entries)
               ;; entries are (BID . OCTETS-BELOW)
               (let ((block (pst-octets (+ 8 (* word (length entries))))))
                 (setf (aref block 0) 1 (aref block 1) level)
                 (pst-put block 2 (length entries) 2)
                 (pst-put block 4 (reduce #'+ entries :key #'cdr) 4)
                 (loop for (bid) in entries for at from 8 by word do (pst-put block at bid word))
                 (cons (pst-block writer block :internal t) (reduce #'+ entries :key #'cdr)))))
      (let ((blocks (mapcar (lambda (chunk) (cons (pst-block writer chunk) (length chunk))) chunks)))
        (cond ((null blocks) 0)
              ((null (rest blocks)) (car (first blocks)))
              ((<= (length blocks) per) (car (listing 1 blocks)))
              (t (let ((xblocks (mapcar (lambda (group) (listing 1 group)) (pst-groups blocks per))))
                   (assert (<= (length xblocks) per))
                   (car (listing 2 xblocks)))))))))

(defun pst-subnode-tree (writer entries)
  "Write the subnode B-tree of ENTRIES, each (NID DATA SUB), the nodes within
a node ([MS-PST] 2.2.2.8.3.3): an SLBLOCK of them, or SLBLOCKs listed by an
SIBLOCK. Returns its BID; 0 for no entries."
  (let* ((word (pst-writer-word writer))
         (head (if (pst-writer-ansi-p writer) 4 8))
         (room (- (pst-writer-room writer) head)))
    (flet ((leaf (group)
             (let ((block (pst-octets (+ head (* 3 word (length group))))))
               (setf (aref block 0) 2 (aref block 1) 0)
               (pst-put block 2 (length group) 2)
               (loop for (nid data sub) in group for at from head by (* 3 word)
                     do (pst-put block at nid word) (pst-put block (+ at word) data word)
                        (pst-put block (+ at word word) sub word))
               (cons (first (first group)) (pst-block writer block :internal t)))))
      (when entries
        (let ((leaves (mapcar #'leaf (pst-groups (sort (copy-list entries) #'< :key #'first) (floor room (* 3 word))))))
          (if (null (rest leaves))
              (return-from pst-subnode-tree (cdr (first leaves)))
              (let ((block (pst-octets (+ head (* 2 word (length leaves))))))
                (assert (<= (length leaves) (floor room (* 2 word))))
                (setf (aref block 0) 2 (aref block 1) 1)
                (pst-put block 2 (length leaves) 2)
                (loop for (nid . bid) in leaves for at from head by (* 2 word)
                      do (pst-put block at nid word) (pst-put block (+ at word) bid word))
                (return-from pst-subnode-tree (pst-block writer block :internal t))))))
      0)))

(defun pst-node (writer nid data sub &optional (parent 0))
  "Enter the node NID in the node B-tree: its data tree DATA and subnode tree
SUB (BIDs, 0 for none), and PARENT, the folder it is in ([MS-PST]
2.2.2.7.7.4)."
  (vector-push-extend (list nid data sub parent) (pst-writer-nodes writer)))

;;; The node database: B-trees, maps and the header, written last

(defun pst-page (writer ptype contents &key at bid)
  "Write a 512-octet page of PTYPE ([MS-PST] 2.2.2.7.1) holding CONTENTS, its
first 496 octets at most (500 in the ANSI format): at AT, where the maps go,
its identifier its offset and its signature 0; else at a new place, its
identifier BID or the next page's, signed. Returns (BID . IB)."
  (let* ((map (member ptype '(#x82 #x83 #x84 #x85)))
         (ansi (pst-writer-ansi-p writer))
         (trailer (if ansi 500 496))
         (ib (or at (pst-allocate writer 512 512)))
         (bid (cond (map ib) (bid)
                    ;; in steps of two, the lowest bit kept clear (2.2.2.2)
                    (t (prog1 (pst-writer-next-page writer) (incf (pst-writer-next-page writer) 2)))))
         (page (pst-octets 512)))
    (replace page contents :end2 (min trailer (length contents)))
    (setf (aref page trailer) ptype (aref page (1+ trailer)) ptype)
    (pst-put page (+ trailer 2) (if map 0 (pst-signature ib bid)) 2)
    (if ansi
        (progn (pst-put page 504 bid 4) (pst-put page 508 (pst-crc page :end trailer) 4))
        (progn (pst-put page 500 (pst-crc page :end trailer) 4) (pst-put page 504 bid 8)))
    (pst-write-at writer page ib)
    (cons bid ib)))

(defun pst-btree (writer ptype count size entry)
  "Write the B-tree of pages of PTYPE (#x80 the block B-tree, #x81 the node
B-tree, [MS-PST] 2.2.2.7.7) over COUNT leaf entries of SIZE octets each, in
order of their keys: ENTRY, a function of an entry's index, a page's
contents and an offset in them, puts that entry there and returns its key.
The leaves are filled and written one at a time, so that no more than a
page of entries is made at once. Returns the root's (BID . IB)."
  (let* ((word (pst-writer-word writer))
         (area (if (pst-writer-ansi-p writer) 496 488))
         (link (* 3 word)))
    (flet ((page (level width count fill)
             (let ((contents (pst-octets (+ area 8))))
               (dotimes (i count) (funcall fill i contents (* i width)))
               (setf (aref contents area) count (aref contents (+ area 1)) (floor area width)
                     (aref contents (+ area 2)) width (aref contents (+ area 3)) level)
               (pst-page writer ptype contents))))
      (let ((pages (loop with per = (floor area size)
                         for first from 0 below (max count 1) by per
                         collect (let* ((key nil)
                                        (place (page 0 size (min per (- count first))
                                                     (lambda (i contents at)
                                                       (let ((k (funcall entry (+ first i) contents at)))
                                                         (when (zerop i) (setf key k)))))))
                                   (cons key place))))
            (level 0))
        (loop while (rest pages)
              do (incf level)
                 (setf pages (loop for group in (pst-groups pages (floor area link))
                                   collect (cons (car (first group))
                                                 (page level link (length group)
                                                       (lambda (i contents at)
                                                         (destructuring-bind (key bid . ib) (nth i group)
                                                           (pst-put contents at key word)
                                                           (pst-put contents (+ at word) bid word)
                                                           (pst-put contents (+ at word word) ib word))))))))
        (cdr (first pages))))))

(defun pst-set-slots (amap from below value)
  "Set the bits of AMAP, an AMap's 496 octets, for its slots from FROM below
BELOW to VALUE, 1 or 0, the most significant bit of an octet its first slot
\([MS-PST] 2.2.2.7.2)."
  (declare (type pst-octets amap) (type fixnum from below) (type bit value))
  (loop with slot of-type fixnum = from
        while (< slot below)
        do (if (and (zerop (mod slot 8)) (<= (+ slot 8) below))
               (progn (setf (aref amap (floor slot 8)) (if (= value 1) #xFF 0)) (incf slot 8))
               (progn (setf (ldb (byte 1 (- 7 (mod slot 8))) (aref amap (floor slot 8))) value) (incf slot)))))

(defun pst-section-amap (amap section end free)
  "Fill AMAP, 496 octets, with the bits of the part of the file SECTION's AMap
maps: a slot taken below END, the first octet not given out, but where it
lies in one of FREE, stretches (START . END) in order, of which those ending
by the part's start are dropped first. Returns the rest of FREE, the free
slots counted and the longest run of them, three values."
  (let ((start (pst-section-start section)) (next (pst-section-start (1+ section))))
    (fill amap 0)
    (pst-set-slots amap 0 (max 0 (min 3968 (ceiling (- end start) 64))) 1)
    (loop while (and free (<= (cdr (first free)) start)) do (pop free))
    (loop for (from . to) in free
          while (< from next)
          do (pst-set-slots amap (max 0 (floor (- from start) 64)) (min 3968 (ceiling (- to start) 64)) 0))
    (let ((count 0) (run 0) (most 0))
      (loop for octet across amap
            do (case octet
                 (#xFF (setf run 0))
                 (0 (incf count 8) (incf run 8) (setf most (max most run)))
                 (t (loop for bit from 7 downto 0
                          do (if (logbitp bit octet)
                                 (setf run 0)
                                 (progn (incf count) (incf run) (setf most (max most run))))))))
      (values free count most))))

(defun pst-write-maps (writer sections)
  "Write the map pages of the SECTIONS parts of the file, one part at a time:
each AMap with a bit for each of its 64-octet slots, set when it is taken
\([MS-PST] 2.2.2.7.2); each page map (PMap) with every bit set, as Outlook
writes them, the density list taking their place (2.2.2.7.3); each free page
map (FPMap) with every bit set, as every PMap says it has no free page
\(2.2.2.7.6); each free map (FMap) with the longest run of free slots of each
AMap from its own part on (2.2.2.7.5). In the ANSI format the bits of an
AMap, a PMap and an FPMap follow four octets of padding. Returns the free
octets counted and each AMap's free slots, a vector."
  (let ((free (reverse (pst-writer-free writer))) (end (pst-writer-end writer))
        (slots (make-array sections)) (longest (pst-octets sections)) (amap (pst-octets 496))
        (full (make-array 496 :element-type '(unsigned-byte 8) :initial-element #xFF))
        (total 0))
    (dotimes (section sections)
      (let ((start (pst-section-start section)))
        (multiple-value-bind (rest count most) (pst-section-amap amap section end free)
          (setf free rest (svref slots section) count (aref longest section) (min 255 most))
          (incf total (* 64 count)))
        (flet ((bits (octets) (if (pst-writer-ansi-p writer) (pst-join (list (pst-octets 4) octets)) octets)))
          (pst-page writer #x84 (bits amap) :at start)
          (when (zerop (mod section 8)) (pst-page writer #x83 (bits full) :at (+ 512 start)))
          (when (pst-fpmap-section-p section) (pst-page writer #x85 (bits full) :at (+ 1024 start))))))
    (loop for section from 128 below sections by 496
          do (pst-page writer #x82 (subseq longest section (min sections (+ section 496)))
                       :at (+ 1024 (pst-section-start section))))
    (values total slots)))

(defun pst-write-density-list (writer slots)
  "Write the density list at #x4200 ([MS-PST] 2.2.2.7.4): the AMaps, at most
119 of them, those with the most free slots (SLOTS, a vector) first."
  (let* ((order (sort (loop for k below (length slots) collect k) #'> :key (lambda (k) (svref slots k))))
         (listed (subseq order 0 (min 119 (length order))))
         (contents (pst-octets 496)))
    (setf (aref contents 0) 1 (aref contents 1) (length listed))
    (pst-put contents 4 (first listed) 4)
    (loop for k in listed for at from 8 by 4
          do (pst-put contents at (logior k (ash (min 4095 (svref slots k)) 20)) 4))
    (pst-page writer #x86 contents :at #x4200)))

(defun pst-finish (writer)
  "Write what ends the mailbox: the node and block B-trees, the maps, the
density list (of the Unicode format alone), and last the header ([MS-PST]
2.2.2.6) pointing at them, with its checks; the file padded to the end of
the last part an AMap maps."
  (let* ((blocks (pst-writer-blocks writer))
         (nodes (sort (pst-writer-nodes writer) #'< :key #'first))
         (word (pst-writer-word writer))
         ;; the blocks are listed in the order of their identifiers, given in turn
         (bbt (pst-btree writer #x80 (floor (length blocks) 3) (* 3 word)
                         (lambda (i page at)
                           (let ((bid (aref blocks (* 3 i))))
                             (pst-put page at bid word)
                             (pst-put page (+ at word) (aref blocks (+ (* 3 i) 1)) word)
                             (pst-put page (+ at word word) (aref blocks (+ (* 3 i) 2)) 2)
                             ;; held by the B-tree and by what refers to it (2.2.2.7.7.3.1)
                             (pst-put page (+ at word word 2) 2 2)
                             bid))))
         (nbt (pst-btree writer #x81 (length nodes) (* 4 word)
                         (lambda (i page at)
                           (destructuring-bind (nid data sub parent) (aref nodes i)
                             (pst-put page at nid word)
                             (pst-put page (+ at word) data word)
                             (pst-put page (+ at word word) sub word)
                             (pst-put page (+ at (* 3 word)) parent 4)
                             nid))))
         (sections (1+ (pst-section-of (1- (pst-writer-end writer)))))
         (eof (pst-section-start sections)))
    (pst-note-free writer (pst-writer-end writer) eof)
    (setf (pst-writer-free writer) (sort (pst-writer-free writer) #'> :key #'car))
    (multiple-value-bind (free slots) (pst-write-maps writer sections)
      (unless (pst-writer-ansi-p writer) (pst-write-density-list writer slots))
      ;; the file reaches its end: the last octet written, the rest a hole
      (let ((last-amap (pst-section-start (1- sections)))
            (crypt (case (pst-writer-encoding writer) (:permute 1) (:cyclic 2) (t 0))))
        (when (< (pst-writer-end writer) eof) (pst-write-at writer (pst-octets 1) (1- eof)))
        (when (pst-writer-ansi-p writer)
          (let ((header (pst-octets 512)))
            (replace header #(#x21 #x42 #x44 #x4E))
            (replace header #(#x53 #x4D) :start1 8)
            (pst-put header 10 14 2)
            (pst-put header 12 19 2)
            (setf (aref header 14) 1 (aref header 15) 1)
            (pst-put header 24 (* 4 (pst-writer-next-block writer)) 4)
            (pst-put header 28 (pst-writer-next-page writer) 4)
            (pst-put header 32 1 4)
            (loop for last across (pst-writer-last-nids writer) for at from 36 by 4 do (pst-put header at last 4))
            ;; the root (2.2.2.5)
            (pst-put header 168 eof 4)
            (pst-put header 172 last-amap 4)
            (pst-put header 176 free 4)
            (pst-put header 184 (car nbt) 4)
            (pst-put header 188 (cdr nbt) 4)
            (pst-put header 192 (car bbt) 4)
            (pst-put header 196 (cdr bbt) 4)
            (setf (aref header 200) 2)
            (fill header #xFF :start 204 :end 460)
            (setf (aref header 460) #x80 (aref header 461) crypt)
            (pst-put header 4 (pst-crc header :start 8 :end (+ 8 471)) 4)
            (pst-write-at writer header 0)
            (return-from pst-finish eof)))
        (let ((header (pst-octets 564)))
          (replace header #(#x21 #x42 #x44 #x4E))
          (replace header #(#x53 #x4D) :start1 8)
          (pst-put header 10 23 2)
          (pst-put header 12 19 2)
          (setf (aref header 14) 1 (aref header 15) 1)
          (pst-put header 32 (pst-writer-next-page writer) 8)
          (pst-put header 40 1 4)
          (loop for last across (pst-writer-last-nids writer) for at from 44 by 4 do (pst-put header at last 4))
          ;; the root (2.2.2.5)
          (pst-put header 184 eof 8)
          (pst-put header 192 last-amap 8)
          (pst-put header 200 free 8)
          (pst-put header 216 (car nbt) 8)
          (pst-put header 224 (cdr nbt) 8)
          (pst-put header 232 (car bbt) 8)
          (pst-put header 240 (cdr bbt) 8)
          (setf (aref header 248) 2)
          (fill header #xFF :start 256 :end 512)
          (setf (aref header 512) #x80 (aref header 513) crypt)
          (pst-put header 516 (* 4 (pst-writer-next-block writer)) 8)
          (pst-put header 4 (pst-crc header :start 8 :end (+ 8 471)) 4)
          (pst-put header 524 (pst-crc header :start 8 :end (+ 8 516)) 4)
          (pst-write-at writer header 0)
          eof)))))

;;; Lists, tables and properties: the heap

(define-record (pst-heap (:constructor make-pst-heap (signature)) (:copier nil))
  "A heap on a node being made ([MS-PST] 2.3.1), for what SIGNATURE says
\(#xBC a property context, #x7C a table context): BLOCKS, a vector of the
items of each of its blocks, each a list latest first, and USED, how many
octets of items each block holds."
  (signature nil :documentation "What PST-HEAP is for: #xBC a property context, #x7C a table context.")
  (blocks (make-array 1 :adjustable t :fill-pointer 1 :initial-element '())
          :documentation "A vector of the items of each block of PST-HEAP, each a list latest first.")
  (used (make-array 1 :adjustable t :fill-pointer 1 :initial-element 0)
        :documentation "How many octets of items each block of PST-HEAP holds."))

(defun pst-heap-header-size (block)
  "The octets of the header the heap's block numbered BLOCK begins with: the
heap's own (HNHDR) in the first, a fill-level map (HNBITMAPHDR) in the
eighth and each 128th after, else HNPAGEHDR ([MS-PST] 2.3.1.2-2.3.1.4)."
  (cond ((zerop block) 12)
        ((and (>= block 8) (zerop (mod (- block 8) 128))) 66)
        (t 2)))

(defun pst-heap-block-size (block used items)
  "The octets the heap's block numbered BLOCK takes with ITEMS items of USED
octets: its header, the items, and the map of them (HNPAGEMAP) after,
aligned on two octets."
  (+ (* 2 (ceiling (+ (pst-heap-header-size block) used) 2)) 4 (* 2 (1+ items))))

(defun pst-heap-add (heap octets)
  "Put OCTETS, at most 3,580 of them, on HEAP, in its last block or a new one
when it would hold more octets than a block may, or more than the 2,047
items its index of 11 bits numbers; return their heap identifier (HID,
[MS-PST] 2.3.1.1)."
  (assert (<= (length octets) 3580))
  (let* ((blocks (pst-heap-blocks heap)) (used (pst-heap-used heap)) (block (1- (length blocks))))
    (when (or (> (pst-heap-block-size block (+ (aref used block) (length octets)) (1+ (length (aref blocks block))))
                 8176)
              (>= (length (aref blocks block)) 2047))
      (vector-push-extend '() blocks)
      (vector-push-extend 0 used)
      (incf block))
    (push octets (aref blocks block))
    (incf (aref used block) (length octets))
    (logior (ash (length (aref blocks block)) 5) (ash block 16))))

(defun pst-fill-level (free)
  "The fill level of a heap's block with FREE octets free, [MS-PST] 2.3.1.2."
  (loop for level from 0
        for least in '(3584 2560 2048 1792 1536 1280 1024 768 512 256 128 64 32 16 8)
        when (>= free least) return level
        finally (return 15)))

(defun pst-heap-chunks (heap root)
  "The blocks of HEAP as they are written, a list of octet vectors, the
heap's own header naming ROOT, the HID of what it holds, as its user root."
  (let* ((blocks (pst-heap-blocks heap))
         (sizes (loop for k below (length blocks)
                      collect (pst-heap-block-size k (aref (pst-heap-used heap) k) (length (aref blocks k)))))
         (levels (mapcar (lambda (size) (pst-fill-level (- 8176 size))) sizes)))
    (loop for k from 0 below (length blocks)
          for size in sizes
          collect (let* ((items (reverse (aref blocks k)))
                         (chunk (pst-octets size))
                         (header (pst-heap-header-size k))
                         (map (- size 4 (* 2 (1+ (length items))))))
                    (pst-put chunk 0 map 2)
                    (cond ((zerop k)
                           (setf (aref chunk 2) #xEC (aref chunk 3) (pst-heap-signature heap))
                           (pst-put chunk 4 root 4)
                           (loop for level in levels for i below 8
                                 do (setf (ldb (byte 4 (* 4 (mod i 2))) (aref chunk (+ 8 (floor i 2)))) level)))
                          ((= header 66)
                           (loop for level in (nthcdr k levels) for i below 128
                                 do (setf (ldb (byte 4 (* 4 (mod i 2))) (aref chunk (+ 2 (floor i 2)))) level))))
                    (pst-put chunk map (length items) 2)
                    (let ((at header))
                      (loop for item in items for i from 0
                            do (pst-put chunk (+ map 4 (* 2 i)) at 2)
                               (replace chunk item :start1 at)
                               (incf at (length item)))
                      (pst-put chunk (+ map 4 (* 2 (length items))) at 2))
                    chunk))))

(defun pst-bth (heap key-size entry-size records)
  "Put on HEAP a B-tree ([MS-PST] 2.3.2) of RECORDS, octet vectors of
KEY-SIZE octets of key then ENTRY-SIZE of data, sorted by key: its header
first, then leaves of as many records as an item holds, and levels of
index records over them as needed. Returns the header's HID."
  (let* ((header (pst-octets 8))
         (hid (pst-heap-add heap header))
         (items (loop for group in (pst-groups records (floor 3580 (+ key-size entry-size)))
                      collect (cons (subseq (first group) 0 key-size) (pst-heap-add heap (pst-join group)))))
         (levels 0))
    (loop while (rest items)
          do (incf levels)
             (setf items (loop for group in (pst-groups items (floor 3580 (+ key-size 4)))
                               collect (cons (car (first group))
                                             (pst-heap-add heap (pst-join (loop for (key . hid) in group
                                                                                collect key
                                                                                collect (pst-number hid 4))))))))
    (setf (aref header 0) #xB5 (aref header 1) key-size (aref header 2) entry-size (aref header 3) levels)
    (pst-put header 4 (if items (cdr (first items)) 0) 4)
    hid))

;;; Lists, tables and properties: property values

(defun pst-local-nids ()
  "A counter of the node numbers given to the nodes within one node: a function
of a node type giving the next number of that type."
  (let ((index #x400))
    (lambda (type) (logior (ash (incf index) 5) type))))

(defun pst-variable-value (writer heap octets nids subnodes)
  "Where a value of OCTETS of any size is kept, as an HNID ([MS-PST] 2.3.3.2):
on HEAP when it fits an item (an empty one too, so that it is there, empty),
else in a node of its own within the node, given a number by NIDS and
entered in SUBNODES, a cons whose car is the list of them."
  (cond ((<= (length octets) 3580) (pst-heap-add heap octets))
        (t (let ((nid (funcall nids #x1F)))
             (push (list nid (pst-data-tree writer (pst-chunks octets)) 0) (car subnodes))
             nid))))

(defun pst-property-context (writer properties nids subnodes)
  "Write PROPERTIES, each (TAG . VALUE) as PST-MESSAGE keeps them, as a
property context ([MS-PST] 2.3.3): a heap holding a B-tree of a record for
each, a value of four octets or fewer in the record, a larger one on the
heap or in a node within the node, an attached message (an object, 2.3.3.5)
in a node within it. NIDS numbers those nodes, entered in SUBNODES, a cons
whose car is the list. Returns the BID of the data tree."
  (let* ((heap (make-pst-heap #xBC))
         (records
           (loop for (tag . value) in (sort (copy-list properties) #'< :key (lambda (p) (ldb (byte 16 16) (car p))))
                 for type = (pst-property-type tag)
                 for size = (pst-fixed-size type)
                 collect (let ((record (pst-octets 8)))
                           (pst-put record 0 (ldb (byte 16 16) tag) 2)
                           (pst-put record 2 type 2)
                           (cond ((and size (<= size 4)) (replace record value :start1 4 :end2 size))
                                 (size (pst-put record 4 (pst-heap-add heap value) 4))
                                 ((eql type #x000D)
                                  (let ((nid (funcall nids #x1F)))
                                    (multiple-value-bind (data sub size) (pst-message-node writer value)
                                      (push (list nid data sub) (car subnodes))
                                      (pst-put record 4 (pst-heap-add heap (pst-join (list (pst-number nid 4)
                                                                                           (pst-number size 4))))
                                               4))))
                                 (t (pst-put record 4 (pst-variable-value writer heap value nids subnodes) 4)))
                           record))))
    (pst-data-tree writer (pst-heap-chunks heap (pst-bth heap 2 6 records)))))

(defun pst-table-context (writer columns rows)
  "Write a table context ([MS-PST] 2.3.4) of COLUMNS, property tags, and
ROWS, each (ROW-ID . PROPERTIES): each row its identifier and version
first, then the values of 8 and 4 octets, of 2, of 1 (a larger or variable
one kept as a property context keeps it, by an HNID), then the bits saying
which are there; the rows in the heap when they fit an item, else in blocks
of a node within it, a whole number of rows in each; and a B-tree from each
row's identifier to its place. Returns the BID of the data tree and that of
the subnode tree, two values."
  (let* ((heap (make-pst-heap #x7C))
         (nids (pst-local-nids))
         (subnodes (list '()))
         (width (lambda (tag)
                  (let ((size (pst-fixed-size (pst-property-type tag))))
                    (if (and size (<= size 8)) size 4))))
         (others (remove-duplicates (remove-if (lambda (tag) (member tag '(#x67F20003 #x67F30003))) columns)
                                    :from-end t))
         (info (pst-octets (+ 22 (* 8 (+ 2 (length others))))))
         (info-hid (pst-heap-add heap info))
         (layout '()) (at 8) (bit 2) (ends '()))
    ;; the row's identifier and version, then the rest by width
    (push (list #x67F20003 0 4 0) layout)
    (push (list #x67F30003 4 4 1) layout)
    (dolist (size '(8 4 2 1))
      (dolist (tag others)
        (when (= size (funcall width tag))
          (push (list tag at size bit) layout)
          (incf at size)
          (incf bit)))
      (unless (= size 8) (push at ends)))
    (let* ((count (length layout))
           (row-size (+ at (ceiling count 8)))
           (rgib (append (reverse ends) (list row-size)))
           (matrix (pst-octets (* row-size (length rows)))))
      (loop for (row-id . properties) in rows
            for base from 0 by row-size
            do (dolist (column layout)
                 (destructuring-bind (tag offset size bit) column
                   (let ((value (cond ((= tag #x67F20003) (pst-number row-id 4))
                                      ((= tag #x67F30003) (pst-number 1 4))
                                      (t (pst-value properties tag)))))
                     (when value
                       (if (pst-fixed-size (pst-property-type tag))
                           (if (<= (pst-fixed-size (pst-property-type tag)) 8)
                               (replace matrix value :start1 (+ base offset) :end2 size)
                               (pst-put matrix (+ base offset) (pst-heap-add heap value) 4))
                           (pst-put matrix (+ base offset) (pst-variable-value writer heap value nids subnodes) 4))
                       (setf (ldb (byte 1 (- 7 (mod bit 8))) (aref matrix (+ base at (floor bit 8)))) 1))))))
      (let* ((rows-hnid
               (cond ((null rows) 0)
                     ((<= (length matrix) 3580) (pst-heap-add heap matrix))
                     (t (let* ((room (pst-writer-room writer))
                               (per-block (floor room row-size))
                               (chunks (loop for start from 0 below (length rows) by per-block
                                             for last = (>= (+ start per-block) (length rows))
                                             collect (let ((piece (subseq matrix (* start row-size)
                                                                          (* (min (length rows) (+ start per-block))
                                                                             row-size))))
                                                       ;; every block but the last is a whole 8 KB (2.3.4.4)
                                                       (if last piece (replace (pst-octets room) piece)))))
                               (nid (funcall nids #x1F)))
                          (push (list nid (pst-data-tree writer chunks) 0) (car subnodes))
                          nid))))
             ;; a row's index in two octets in the ANSI format (2.3.4.3)
             (index-size (if (pst-writer-ansi-p writer) 2 4))
             (index (pst-bth heap 4 index-size (mapcar (lambda (pair)
                                                         (pst-join (list (pst-number (car pair) 4)
                                                                         (pst-number (cdr pair) index-size))))
                                                       (sort (loop for (row-id) in rows for i from 0
                                                                   collect (cons row-id i))
                                                             #'< :key #'car)))))
        (setf (aref info 0) #x7C (aref info 1) count)
        (loop for end in rgib for k from 2 by 2 do (pst-put info k end 2))
        (pst-put info 10 index 4)
        (pst-put info 14 rows-hnid 4)
        (loop for (tag offset size bit) in (sort (copy-list layout) #'< :key #'first)
              for k from 22 by 8
              do (pst-put info k tag 4) (pst-put info (+ k 4) offset 2)
                 (setf (aref info (+ k 6)) size (aref info (+ k 7)) bit))
        (values (pst-data-tree writer (pst-heap-chunks heap info-hid))
                (pst-subnode-tree writer (car subnodes)))))))

;;; Messaging: messages

(defparameter *pst-recipient-columns*
  '(#x0C150003 #x0E0F000B #x0FF90102 #x0FFE0003 #x0FFF0102 #x3001001F #x3002001F #x3003001F #x300B0102
    #x39000003 #x39FF001F #x3A40000B)
  "The columns every recipient table has ([MS-PST] 2.4.5.3.1), the row's
identifier and version apart.")

(defparameter *pst-attachment-columns*
  '(#x0E200003 #x3704001F #x37050003 #x370B0003)
  "The columns every attachment table has ([MS-PST] 2.4.6.1.1), the row's
identifier and version apart.")

(defparameter *pst-contents-columns*
  '(#x00170003 #x001A001F #x00360003 #x0037001F #x00390040 #x0042001F #x0057000B #x0058000B #x0070001F
    #x00710102 #x0E03001F #x0E04001F #x0E060040 #x0E070003 #x0E080003 #x0E170003 #x0E300003 #x0E330014
    #x0E340102 #x0E380003 #x0E3C0102 #x0E3D0102 #x10970003 #x30080040 #x65C60003)
  "The columns every folder's contents table has, its messages' properties
copied into them ([MS-PST] 2.4.4.5.1), the row's identifier and version
apart.")

(defparameter *pst-hierarchy-columns*
  '(#x0E300003 #x0E330014 #x0E340102 #x0E380003 #x3001001F #x36020003 #x36030003 #x360A000B #x3613001F
    #x66350003 #x66360003)
  "The columns every folder's hierarchy table has, its folders' properties
copied into them ([MS-PST] 2.4.4.4.1; the container class a string, as
Outlook keeps it), the row's identifier and version apart.")

(defparameter *pst-associated-columns*
  '(#x001A001F #x0E070003 #x0E170003 #x3001001F #x6800001F #x6803000B #x68051003 #x70030003 #x70040102
    #x70050102 #x7006001F #x70070003)
  "The columns every folder's table of associated messages has ([MS-PST]
2.4.4.6.1), the row's identifier and version apart.")

(defparameter *pst-search-contents-columns*
  '(#x00170003 #x001A001F #x00360003 #x0E070003 #x0E170003 #x0037001F #x0042001F #x0057000B #x0E03001F
    #x0E04001F #x0E05001F #x0E060040 #x0E080003 #x0E2A000B #x30080040 #x67F10003)
  "The columns of the contents table of a search folder ([MS-PST]
2.4.8.6.2.1), the row's identifier and version apart.")

(defun pst-with-defaults (properties defaults)
  "PROPERTIES with each of DEFAULTS, (TAG . VALUE) as PST-PROPERTY takes them,
that PROPERTIES lack."
  (append properties
          (loop for (tag . value) in defaults
                unless (assoc tag properties) collect (pst-property tag value))))

(defun pst-message-size (message)
  "About how many octets MESSAGE takes: its values, its recipients' and its
attachments', an attached message's counted alike."
  (labels ((size (properties)
             (loop for (nil . value) in properties
                   sum (if (pst-message-p value) (pst-message-size value) (length value)))))
    (+ (size (pst-message-properties message))
       (reduce #'+ (pst-message-recipients message) :key #'size)
       (reduce #'+ (pst-message-attachments message) :key #'size))))

(defun pst-message-node (writer message)
  "Write MESSAGE as a message's node ([MS-PST] 2.4.5): its properties, with
those every message has added where it lacks them (2.4.5.1.1), a property
context in the data; within the node its table of recipients, its
attachments, each a property context, and their table (2.4.6), and its
larger values. Returns four values: the BIDs of its data tree and subnode
tree, its size, and its properties as written."
  (let* ((nids (pst-local-nids))
         (subnodes (list '()))
         (given (pst-message-properties message))
         (attachments (pst-message-attachments message))
         (size (pst-message-size message))
         (time (or (pst-value given #x0E060040) (pst-value given #x00390040)
                   (pst-number (pst-filetime (get-universal-time)) 8)))
         (flags (let ((value (pst-value given #x0E070003)))
                  (if attachments
                      (logior #x10 (if value (octets-integer value 0 4) 0))
                      (logandc2 (if value (octets-integer value 0 4) 0) #x10))))
         (properties (pst-with-defaults
                      (append (remove-if (lambda (p) (member (car p) '(#x0E070003 #x0E080003 #x0E1B000B))) given)
                              (list (pst-property #x0E070003 flags) (pst-property #x0E080003 size)
                                    (pst-property #x0E1B000B (and attachments t))))
                      (list (cons #x001A001F "IPM.Note") (cons #x0E170003 0) (cons #x0E060040 time)
                            (cons #x30070040 time) (cons #x30080040 time)
                            (cons #x300B0102 (let ((key (pst-octets 16)))
                                               (dotimes (i 16 key) (setf (aref key i) (random 256)))))))))
    (let ((recipients (mapcar (lambda (properties)
                                (pst-with-defaults properties
                                                   (list (cons #x0C150003 1) (cons #x0FFE0003 6) (cons #x39000003 0))))
                              (pst-message-recipients message))))
      (multiple-value-bind (data sub)
          (pst-table-context writer
                             (union *pst-recipient-columns*
                                    (remove-duplicates (mapcar #'car (reduce #'append recipients))))
                             (loop for properties in recipients for row from 1 collect (cons row properties)))
        (push (list #x692 data sub) (car subnodes))))
    (when attachments
      (let ((rows (loop for given in attachments
                        collect (let* ((nid (funcall nids 5))
                                       (data (pst-value given #x37010102))
                                       (properties (pst-with-defaults
                                                    given
                                                    (list (cons #x0E200003 (if data (length data) 0))
                                                          (cons #x37050003 1) (cons #x370B0003 #xFFFFFFFF))))
                                       (own (list '()))
                                       (pc (pst-property-context writer properties (pst-local-nids) own)))
                                  (push (list nid pc (pst-subnode-tree writer (car own))) (car subnodes))
                                  (cons nid properties)))))
        (multiple-value-bind (data sub)
            (pst-table-context writer (append *pst-attachment-columns* '(#x3707001F #x370E001F)) rows)
          (push (list #x671 data sub) (car subnodes)))))
    (let ((data (pst-property-context writer properties nids subnodes)))
      (values data (pst-subnode-tree writer (car subnodes)) size properties))))

;;; Messaging: folders and the message store

(define-record (pst-folder (:constructor make-pst-folder (name index &key class (type 2))) (:copier nil))
  "A folder of a mailbox being written: NAME as shown, INDEX the index of its
node numbers, TYPE that of its properties' node (2, or 3 for a search
folder), CLASS the kind of items it holds (IPF.Note for mail) or NIL;
CHILDREN its folders, ROWS its contents table's rows, latest first, each
\(NID . PROPERTIES), and UNREAD how many of them are not read."
  (name nil :documentation "The name of PST-FOLDER as shown.")
  (index nil :documentation "The index of the node numbers of PST-FOLDER.")
  (class nil :documentation "The kind of items PST-FOLDER holds (IPF.Note for mail), or NIL.")
  (type nil :documentation "The type of the node of the properties of PST-FOLDER: 2, or 3 for a search folder.")
  (children '() :documentation "The folders in PST-FOLDER.")
  (rows '() :documentation "The rows of the contents table of PST-FOLDER, latest first, each (NID . PROPERTIES).")
  (unread 0 :documentation "How many of the rows of PST-FOLDER are not read."))

(defun pst-folder-under (writer folder names)
  "The folder of FOLDER's folders at the path NAMES, a list of names, made
where it is not there yet, a folder of mail."
  (if (null names)
      folder
      (let ((child (or (find (first names) (pst-folder-children folder) :key #'pst-folder-name :test #'string=)
                       (let* ((last (pst-writer-last-nids writer))
                              (index (incf (aref last 2))))
                         (dolist (type '(#xD #xE #xF)) (setf (aref last type) index))
                         (car (last (setf (pst-folder-children folder)
                                          (append (pst-folder-children folder)
                                                  (list (make-pst-folder (first names) index :class "IPF.Note"))))))))))
        (pst-folder-under writer child (rest names)))))

(defun pst-copied (properties columns)
  "Those of PROPERTIES whose tags are among COLUMNS: what a table's row
copies of the object it lists."
  (remove-if-not (lambda (property) (member (car property) columns)) properties))

(defun pst-write-folder (writer folder parent)
  "Write FOLDER, in the folder whose properties' node number is PARENT, and
the folders in it: its properties (2.4.4.1), its hierarchy table listing its
folders but search folders, its contents table listing its messages, its empty table of
associated messages ([MS-PST] 2.4.4). A search folder is its properties
alone. Returns the properties written, for the table of the folder it is
in."
  (let* ((index (pst-folder-index folder))
         (nid (pst-folder-nid index (pst-folder-type folder)))
         (children (mapcar (lambda (child)
                             (cons (pst-folder-nid (pst-folder-index child) (pst-folder-type child))
                                   (pst-write-folder writer child nid)))
                           (pst-folder-children folder)))
         (properties (append (list (pst-property #x3001001F (pst-folder-name folder))
                                   (pst-property #x36020003 (length (pst-folder-rows folder)))
                                   (pst-property #x36030003 (pst-folder-unread folder))
                                   (pst-property #x360A000B (and children t)))
                             (and (pst-folder-class folder)
                                  (list (pst-property #x3613001F (pst-folder-class folder)))))))
    (pst-node writer nid (pst-property-context writer properties (pst-local-nids) (list '())) 0 parent)
    (when (= 2 (pst-folder-type folder))
      (loop for (type columns rows)
              in (list (list #xD *pst-hierarchy-columns*
                             ;; a search folder is not listed: the root lists two folders (2.7.1)
                             (loop for (nid . properties) in children
                                   unless (= 3 (logand nid #x1F))
                                     collect (cons nid (pst-copied properties *pst-hierarchy-columns*))))
                       (list #xE *pst-contents-columns* (reverse (pst-folder-rows folder)))
                       (list #xF *pst-associated-columns* '()))
            do (multiple-value-bind (data sub) (pst-table-context writer columns rows)
                 (pst-node writer (pst-folder-nid index type) data sub))))
    properties))

(defun pst-entry-id (record-key nid)
  "The entry identifier of the node NID in the mailbox whose record key is
RECORD-KEY ([MS-PST] 2.4.3.2): four octets of flags, 0, the key, the
number."
  (pst-join (list (pst-octets 4) record-key (pst-number nid 4))))

(defun pst-write-store (writer name record-key)
  "Write what every mailbox holds but its folders ([MS-PST] 2.7): the message
store called NAME with its record key RECORD-KEY and where its top folder,
deleted items and search folders are (2.4.3.1); the map of the named
properties of the messages written (PST-WRITER-NAMES, 2.4.7, 2.7.3.2); the
tables every new folder, recipient table
and attachment table copies its columns from (2.7.3.3); and the empty
queues of searching (2.7.3.5)."
  (flet ((pc (nid properties)
           (pst-node writer nid (pst-property-context writer properties (pst-local-nids) (list '())) 0))
         (template (nid columns)
           (multiple-value-bind (data sub) (pst-table-context writer columns '())
             (pst-node writer nid data sub))))
    (pc #x21 (list (pst-property #x0FF90102 record-key) (pst-property #x3001001F name)
                   (pst-property #x35DF0003 #x89)
                   (pst-property #x35E00102 (pst-entry-id record-key #x8022))
                   (pst-property #x35E30102 (pst-entry-id record-key #x8062))
                   (pst-property #x35E70102 (pst-entry-id record-key #x8042))
                   (pst-property #x67FF0003 0)))
    (multiple-value-bind (guids entries strings buckets) (pst-name-streams (pst-writer-names writer))
      (pc #x61 (append (list (pst-property #x00010003 251) (pst-property #x00020102 guids)
                             (pst-property #x00030102 entries) (pst-property #x00040102 strings))
                       (loop for (bucket . held) in buckets
                             collect (pst-property (logior (ash (+ #x1000 bucket) 16) #x0102) held)))))
    (template #x60D *pst-hierarchy-columns*)
    (template #x60E *pst-contents-columns*)
    (template #x60F *pst-associated-columns*)
    (template #x610 *pst-search-contents-columns*)
    (template #x671 *pst-attachment-columns*)
    (template #x692 *pst-recipient-columns*)
    (pst-node writer #x1E1 0 0)
    (pst-node writer #x201 0 0)))

(defun write-pst (fd path name folders &key (encoding :permute) (format :unicode) (each #'identity))
  "Write a new mailbox ([MS-PST]) into the empty file open as file descriptor
\(FD) at PATH, NAME the name its top folder is shown by, holding FOLDERS, each
\(NAMES . SOURCES): NAMES the path of the folder under the top of the
mailbox, a list of names, and SOURCES what EACH, a function, makes a
PST-MESSAGE of, or NIL to leave out, its named properties numbered anew for
the mailbox (PST-RENAMED); each is made and written in turn, the
message and its attachments in the file before the next is made. Beside them
the folders every mailbox has: the top of the personal folders, deleted
items, the root of searches and a search folder of spam (2.7.2). Data
blocks are permuted, with ENCODING :cyclic encoded by NDB_CRYPT_CYCLIC, with
:none written as they are; with FORMAT :ansi the mailbox is of the ANSI
format of Outlook 97 to 2002, which the tests of the reader take. Returns
the file's size."
  (let* ((writer (make-pst-writer fd path :encoding encoding :format format))
         (*random-state* (make-random-state t))
         (record-key (let ((key (pst-octets 16))) (dotimes (i 16 key) (setf (aref key i) (random 256)))))
         (top (make-pst-folder "Top of Personal Folders" #x401))
         (root (make-pst-folder "" 9)))
    (setf (pst-folder-children top) (list (make-pst-folder "Deleted Items" #x403 :class "IPF.Note"))
          (pst-folder-children root) (list top (make-pst-folder "Search Root" #x402)
                                           (make-pst-folder "SPAM Search Folder 2" #x111 :type 3)))
    (loop for (names . sources) in folders
          for folder = (pst-folder-under writer top names)
          do (dolist (source sources)
               (let ((message (let ((made (funcall each source)))
                                (and made (pst-renamed made (pst-writer-names writer))))))
                 (when message
                   (let ((nid (pst-new-nid writer 4)))
                     (multiple-value-bind (data sub size properties) (pst-message-node writer message)
                       (declare (ignore size))
                       (pst-node writer nid data sub (pst-folder-nid (pst-folder-index folder) 2))
                       (push (cons nid (pst-copied properties *pst-contents-columns*)) (pst-folder-rows folder))
                       (unless (logtest 1 (octets-integer (pst-value properties #x0E070003) 0 4))
                         (incf (pst-folder-unread folder)))))))))
    ;; the root folder is in itself (2.7.3.4.1)
    (pst-write-folder writer root #x122)
    (pst-write-store writer name record-key)
    (pst-finish writer)))
