;;;; test.lisp --- The outlook package: Outlook items, saved mail and mailboxes read safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test outlook
  :description "The outlook package: compound files read with every chain checked (loops, streams larger than
their chains, items nested deep or in a circle refused or cut short, never hanging); Outlook items read as
messages (sender, recipients, dates, plain, HTML and compressed RTF bodies, internet headers, attachments, an
attached item as a message, what the item says of itself); messages drawn as they look, shut in, nothing
fetched; internet mail (encoded words, quoted-printable,
base64, parts within parts, a boundary that never comes); attachments opened from private copies, the
original untouched; mailboxes listed from what readpst writes, and refused with how to get readpst without
it; previews, health and load-package.")

;;; A compound file written, for the tests

(defstruct (cfbw-entry (:copier nil))
  "An entry of a compound file being written for the tests."
  name type octets child right (start #xFFFFFFFE) (size 0))

(defun cfb-test-octets (tree)
  "The octets of a compound file (sectors of 512 octets) holding TREE: a
storage (:storage NAME CHILD...) or a stream (:stream NAME OCTETS), the
first the root. Streams below 4,096 octets go in the mini stream."
  (let ((entries (make-array 0 :adjustable t :fill-pointer 0))
        (mini (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (mini-fat '()) (big '()))
    (labels ((add (node)
               (let* ((entry (make-cfbw-entry :name (second node) :type (if (eq (first node) :stream) 2 1)))
                      (index (vector-push-extend entry entries)))
                 (if (eq (first node) :stream)
                     (let ((octets (coerce (third node) '(vector (unsigned-byte 8)))))
                       (setf (cfbw-entry-octets entry) octets (cfbw-entry-size entry) (length octets)))
                     (let ((children (mapcar #'add (cddr node))))
                       (setf (cfbw-entry-child entry) (first children))
                       (loop for (a b) on children while b do (setf (cfbw-entry-right (aref entries a)) b))))
                 index)))
      (add tree)
      (setf (cfbw-entry-type (aref entries 0)) 5))
    ;; the small streams into the mini stream, the others in sectors of their own
    (loop for entry across entries
          for size = (cfbw-entry-size entry)
          when (and (= 2 (cfbw-entry-type entry)) (plusp size) (< size 4096))
            do (let ((first (floor (length mini) 64)) (count (ceiling size 64)))
                 (setf (cfbw-entry-start entry) first)
                 (loop for k below count do (push (if (< k (1- count)) (+ first k 1) #xFFFFFFFE) mini-fat))
                 (loop for o across (cfbw-entry-octets entry) do (vector-push-extend o mini))
                 (loop repeat (- (* 64 count) size) do (vector-push-extend 0 mini)))
          when (and (= 2 (cfbw-entry-type entry)) (>= size 4096))
            do (push entry big))
    (setf mini-fat (coerce (nreverse mini-fat) 'vector) big (nreverse big))
    (let* ((dir-count (ceiling (length entries) 4))
           (mini-fat-count (ceiling (length mini-fat) 128))
           (mini-count (ceiling (length mini) 512))
           (big-count (reduce #'+ big :key (lambda (e) (ceiling (cfbw-entry-size e) 512))))
           (others (+ dir-count mini-fat-count mini-count big-count))
           (fat-count (loop for f from 1 when (>= (* f 128) (+ others f)) return f))
           (total (+ fat-count others))
           (out (make-array (* 512 (1+ total)) :element-type '(unsigned-byte 8) :initial-element 0))
           (fat (make-array (* fat-count 128) :initial-element #xFFFFFFFF))
           (next fat-count))
      (labels ((put (at value count) (tuatara.io:octets-put-integer out value count :at at))
               (run (count) (let ((start next))
                              (dotimes (k count)
                                (setf (aref fat (+ start k)) (if (< k (1- count)) (+ start k 1) #xFFFFFFFE)))
                              (incf next count)
                              (if (plusp count) start #xFFFFFFFE)))
               (sector (n) (* 512 (1+ n))))
        (dotimes (k fat-count) (setf (aref fat k) #xFFFFFFFD))
        (let* ((dir-start (run dir-count))
               (mini-fat-start (run mini-fat-count))
               (mini-start (run mini-count)))
          (dolist (entry big) (setf (cfbw-entry-start entry) (run (ceiling (cfbw-entry-size entry) 512))))
          (setf (cfbw-entry-start (aref entries 0)) (if (plusp (length mini)) mini-start #xFFFFFFFE)
                (cfbw-entry-size (aref entries 0)) (length mini))
          ;; the header
          (replace out #(#xD0 #xCF #x11 #xE0 #xA1 #xB1 #x1A #xE1))
          (put 24 #x3E 2) (put 26 3 2) (put 28 #xFFFE 2) (put 30 9 2) (put 32 6 2)
          (put 44 fat-count 4) (put 48 dir-start 4) (put 56 4096 4)
          (put 60 (if (plusp mini-fat-count) mini-fat-start #xFFFFFFFE) 4) (put 64 mini-fat-count 4)
          (put 68 #xFFFFFFFE 4) (put 72 0 4)
          (dotimes (k 109) (put (+ 76 (* 4 k)) (if (< k fat-count) k #xFFFFFFFF) 4))
          ;; the FAT, the directory, the mini FAT, the mini stream, the large streams
          (loop for value across fat for k from 0 do (put (+ (sector 0) (* 4 k)) value 4))
          (loop for entry across entries
                for at from (sector dir-start) by 128
                do (let ((name (sb-ext:string-to-octets (cfbw-entry-name entry) :external-format :utf-16le)))
                     (replace out name :start1 at)
                     (put (+ at 64) (+ 2 (length name)) 2)
                     (put (+ at 66) (cfbw-entry-type entry) 1)
                     (put (+ at 67) 1 1)
                     (put (+ at 68) #xFFFFFFFF 4)
                     (put (+ at 72) (or (cfbw-entry-right entry) #xFFFFFFFF) 4)
                     (put (+ at 76) (or (cfbw-entry-child entry) #xFFFFFFFF) 4)
                     (put (+ at 116) (cfbw-entry-start entry) 4)
                     (put (+ at 120) (cfbw-entry-size entry) 8)))
          ;; unused entries of the last directory sector: no links
          (loop for k from (length entries) below (* 4 dir-count)
                do (let ((at (+ (sector dir-start) (* 128 k))))
                     (put (+ at 68) #xFFFFFFFF 4)
                     (put (+ at 72) #xFFFFFFFF 4)
                     (put (+ at 76) #xFFFFFFFF 4)))
          (loop for value across mini-fat for k from 0 do (put (+ (sector mini-fat-start) (* 4 k)) value 4))
          (replace out mini :start1 (sector mini-start))
          (dolist (entry big) (replace out (cfbw-entry-octets entry) :start1 (sector (cfbw-entry-start entry))))))
      out)))

;;; An Outlook item written, for the tests

(defun utf-16-octets (text)
  "TEXT as UTF-16, least significant octet first."
  (sb-ext:string-to-octets text :external-format :utf-16le))

(defun msg-test-storage (name properties &key (header 32) children)
  "A storage called NAME (:storage ...) for CFB-TEST-OCTETS laid out as an
Outlook item's: PROPERTIES, each (ID TYPE VALUE), TYPE #x001F a string,
#x0102 octets, #x0003 a number, #x000B a boolean, #x0040 a universal time,
#x000D a list of the nodes of a storage within; HEADER octets before the
fixed-size properties; CHILDREN, more nodes in it."
  (let ((fixed (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (streams '()))
    (dotimes (k header) (vector-push-extend 0 fixed))
    (flet ((entry (id type value)
             (let ((octets (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
               (tuatara.io:octets-put-integer octets (logior type (ash id 16)) 4 :at 0)
               (setf (aref octets 4) 6)
               (tuatara.io:octets-put-integer octets value 8 :at 8)
               (loop for o across octets do (vector-push-extend o fixed)))))
      (loop for (id type value) in properties
            for stream-name = (format nil "__substg1.0_~4,'0X~4,'0X" id type)
            do (ecase type
                 (#x001F (let ((octets (concatenate '(vector (unsigned-byte 8)) (utf-16-octets value) #(0 0))))
                           (push (list :stream stream-name octets) streams)
                           (entry id type (length octets))))
                 (#x0102 (push (list :stream stream-name value) streams) (entry id type (length value)))
                 (#x0003 (entry id type (ldb (byte 32 0) value)))
                 (#x000B (entry id type (if value 1 0)))
                 (#x0040 (entry id type (* (+ value 9435484800) 10000000)))
                 (#x000D (push (list* :storage stream-name value) streams) (entry id type #xFFFFFFFF)))))
    (list* :storage name (list :stream "__properties_version1.0" (coerce fixed '(vector (unsigned-byte 8))))
           (append (nreverse streams) children))))

(defparameter *png-test-octets*
  (coerce '(#x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A #x00 #x00 #x00 #x0D #x49 #x48 #x44 #x52 #x00 #x00 #x00 #x01
            #x00 #x00 #x00 #x01 #x08 #x06 #x00 #x00 #x00 #x1F #x15 #xC4 #x89 #x00 #x00 #x00 #x0A #x49 #x44 #x41
            #x54 #x78 #x9C #x63 #x00 #x01 #x00 #x00 #x05 #x00 #x01 #x0D #x0A #x2D #xB4 #x00 #x00 #x00 #x00 #x49
            #x45 #x4E #x44 #xAE #x42 #x60 #x82)
          '(vector (unsigned-byte 8)))
  "A picture of one pixel, as a PNG.")

(defparameter *msg-test-sent* (encode-universal-time 0 15 20 6 10 2026 0)
  "When the test item was sent: 2026-10-06 20:15 UTC.")

(defun lzfu-test-octets (tokens)
  "Compressed RTF (LZFu) made of TOKENS, each an octet written as it is or
(OFFSET LENGTH), a reference into the dictionary; its end marked."
  (let ((body (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (write (length outlook::*lzfu-dictionary*))
        (raw 0)
        (tokens (append tokens (list :end))))
    (loop while tokens
          do (let ((control 0) (group '()))
               (dotimes (bit 8)
                 (let ((token (pop tokens)))
                   (when (null token) (return))
                   (cond ((eq token :end) (setf control (logior control (ash 1 bit))) (push (list (ash write 4)) group))
                         ((consp token)
                          (setf control (logior control (ash 1 bit)))
                          (push (list (logior (ash (first token) 4) (- (second token) 2))) group)
                          (incf write (second token)) (incf raw (second token)))
                         (t (push token group) (incf write) (incf raw)))
                   (setf write (logand write 4095))))
               (vector-push-extend control body)
               (dolist (item (nreverse group))
                 (if (consp item)
                     (tuatara.io:octets-put-integer body (first item) 2 :big-endian t)
                     (vector-push-extend item body)))))
    (let ((out (make-array (+ 16 (length body)) :element-type '(unsigned-byte 8) :initial-element 0)))
      (flet ((put (at value) (tuatara.io:octets-put-integer out value 4 :at at)))
        (put 0 (+ 12 (length body))) (put 4 raw) (put 8 #x75465A4C))
      (replace out body :start1 16)
      out)))

(defun rtf-test-tokens (text)
  "TEXT's octets as LZFu tokens written as they are."
  (coerce (sb-ext:string-to-octets text :external-format :latin-1) 'list))

(defun msg-test-octets (&key (body :plain) (inner-depth 1))
  "An Outlook item for the tests: sent by Kea Counter, to two, copied to one,
blind copied to one; its BODY plain text, HTML alone (:html) or compressed
RTF alone (:rtf); its internet headers; two attachments, a PNG and an
attached item nested INNER-DEPTH deep."
  (labels ((inner (depth)
             (msg-test-storage "__substg1.0_3701000D"
                               `((#x0037 #x001F ,(format nil "Inner subject ~d" depth))
                                 (#x001A #x001F "IPM.Note")
                                 (#x1000 #x001F ,(format nil "Inner body ~d" depth)))
                               :header 24
                               :children (when (< depth inner-depth)
                                           (list (msg-test-storage "__attach_version1.0_#00000000"
                                                                   '((#x3705 #x0003 5))
                                                                   :header 8 :children (list (inner (1+ depth)))))))))
    (cfb-test-octets
     (msg-test-storage
      "Root Entry"
      (append
       `((#x0037 #x001F "Kea survey results")
         (#x001A #x001F "IPM.Note")
         (#x0E07 #x0003 9)
         (#x0039 #x0040 ,*msg-test-sent*)
         (#x0E06 #x0040 ,(+ *msg-test-sent* 60))
         (#x0C1A #x001F "Kea Counter")
         (#x0C1F #x001F "/O=EXAMPLE/OU=FIRST/CN=RECIPIENTS/CN=KEA")
         (#x0C1E #x001F "EX")
         (#x5D01 #x001F "kea@example.org")
         (#x1035 #x001F "<survey-1@example.org>")
         (#x007D #x001F
                 ,(format nil
                          "Received: from mail.example.org by mx.example.net; Tue, 6 Oct 2026 20:15:30 +0000~c~cX-Origin: tested~c~c"
                                 #\Return #\Newline #\Return #\Newline)))
       (ecase body
         (:plain '((#x1000 #x001F "Twelve kea counted at dawn.")))
         (:html `((#x1013 #x0102
                          ,(sb-ext:string-to-octets "<html><head><meta charset='utf-8'><style>p{}</style><script>alert(1)</script></head><body><p>Kea &amp; kaka seen</p><p><a href='https://example.org/count'>the count</a></p><img src='https://example.org/track.png' alt='tracker'></body></html>"
                                                    :external-format
                                                    :utf-8))))
         (:rtf `((#x1009 #x0102
                         ,(lzfu-test-octets (append (list '(0 11))
                                                    (rtf-test-tokens "\\ansicpg1252 {\\fonttbl{\\f0 Arial;}}Kea \\'e9t\\'e9\\par \\u8364?done}"))))))))
      :children
      (list (msg-test-storage "__recip_version1.0_#00000000"
                              '((#x3001 #x001F "Ruru Canterbury") (#x39FE #x001F "ruru@example.org") (#x0C15 #x0003 1))
                              :header 8)
            (msg-test-storage "__recip_version1.0_#00000001"
                              '((#x3001 #x001F "Tui Rotorua") (#x39FE #x001F "tui@example.org") (#x0C15 #x0003 1))
                              :header 8)
            (msg-test-storage "__recip_version1.0_#00000002"
                              '((#x3001 #x001F "Weka Southland") (#x39FE #x001F "weka@example.org") (#x0C15 #x0003 2))
                              :header 8)
            (msg-test-storage "__recip_version1.0_#00000003"
                              '((#x3001 #x001F "Hidden Hoiho") (#x39FE #x001F "hoiho@example.org") (#x0C15 #x0003 3))
                              :header 8)
            (msg-test-storage "__attach_version1.0_#00000000"
                              `((#x3707 #x001F "kea.png") (#x3705 #x0003 1) (#x3701 #x0102 ,*png-test-octets*))
                              :header 8)
            (msg-test-storage "__attach_version1.0_#00000001" '((#x3705 #x0003 5)) :header 8 :children (list (inner 1))))))))

(defun write-test-octets (path octets)
  "Write OCTETS as the file at PATH; return PATH as a native string."
  (with-open-file (out path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
    (write-sequence octets out))
  (namestring path))

(defun mail-test-text (mail)
  "MAIL as its buffer shows it."
  (values (tuatara.defaults::pieces-text (outlook::mail-pieces mail))))

(defun has (text part)
  "Check that TEXT holds PART."
  (true (search part text) "~s in~%~a" part text))

(defun point-to-line-of (buffer text)
  "Put the point of BUFFER at the start of its first line holding TEXT, so
that a test reaches a line of a list however many lines stand above it."
  (let* ((whole (tuatara.buffer:buffer-string buffer))
         (at (search text whole)))
    (assert at () "~s not in~%~a" text whole)
    (setf (tuatara.buffer:buffer-point buffer)
          (let ((start (position #\Newline whole :end at :from-end t)))
            (if start (1+ start) 0)))))

;;; Compound files

(define-test (outlook item-read)
    (let* ((mail (outlook::msg-read (msg-test-octets)))
         (text (mail-test-text mail)))
      (is string= "Kea survey results" (outlook::mail-subject mail))
      (is string= "Kea Counter <kea@example.org>" (outlook::mail-from mail))
      (is string= "Ruru Canterbury <ruru@example.org>, Tui Rotorua <tui@example.org>" (outlook::mail-to mail))
      (is string= "Weka Southland <weka@example.org>" (outlook::mail-cc mail))
      (is string= "Hidden Hoiho <hoiho@example.org>" (outlook::mail-bcc mail))
      (is = *msg-test-sent* (outlook::mail-sent mail))
      (is = (+ 60 *msg-test-sent*) (outlook::mail-received mail))
      (is eq :plain (outlook::mail-body-kind mail))
    (has text "Twelve kea counted at dawn.")
    (has text "Message-ID  <survey-1@example.org>")
    (has text "2026-10-06 20:15:00 UTC")
    ;; what an examiner looks for: the item's class, its flags, the sender as the item has it
    (has text "Item: IPM.Note")
    (has text "Flags: read, unsent (a draft)")
    (has text "Sender as the item says: Kea Counter </O=EXAMPLE/OU=FIRST/CN=RECIPIENTS/CN=KEA>")
    (has text "Sender address type: EX")
    (has text "Bodies it holds: plain text")
    (has text "Internet headers")
    (has text "Received: from mail.example.org")
    ;; its attachments: a picture by its contents, and an item attached, read as a message
    (has text "Attachments: 2")
    (has text "kea.png  67 B, PNG image, 1 × 1, 8-bit RGBA")
    (has text "Inner subject 1  a message")
      (let ((inner (outlook::mail-attachment-message (second (outlook::mail-attachments mail)))))
      (true inner)
        (is string= "Inner body 1" (outlook::mail-body inner)))
    ;; the brief text a preview shows
      (let ((brief (outlook::mail-brief-text mail)))
      (has brief "Kea survey results")
      (false (search "Internet headers" brief)))))

(define-test (outlook item-bodies)
  ;; HTML alone: read as text, nothing fetched, the script left out, the link's address said
    (let* ((mail (outlook::msg-read (msg-test-octets :body :html)))
           (body (outlook::mail-body mail)))
      (is eq :html (outlook::mail-body-kind mail))
    (has body "Kea & kaka seen")
    (has body "the count <https://example.org/count>")
    (has body "[picture: tracker; from https://example.org/track.png, not fetched]")
    (false (search "alert" body))
    (has (mail-test-text mail) "Text (HTML read as text)")
    (has (funcall (outlook::mail-html mail)) "<script>alert(1)</script>"))
  ;; RTF that keeps HTML gives it back; RTF of its own has none; plain text alone has none
  (multiple-value-bind (text html)
      (outlook::rtf-text (tuatara.text:string-octets "{\\rtf1\\ansi\\fromhtml1 {\\*\\htmltag64 <p style=\"color:red\">}Kea{\\*\\htmltag72 </p>}\\htmlrtf {\\par}\\htmlrtf0 }"))
    (is string= "Kea" (string-trim '(#\Newline) text))
    (is string= "<p style=\"color:red\">Kea</p>" (string-trim '(#\Space) html)))
  (false (funcall (outlook::mail-html (outlook::msg-read (msg-test-octets :body :rtf)))))
  (false (outlook::mail-html (outlook::msg-read (msg-test-octets))))
  ;; compressed RTF alone: taken out, a reference into the dictionary followed, its text read
  (is = 207 (length outlook::*lzfu-dictionary*))
  (let* ((mail (outlook::msg-read (msg-test-octets :body :rtf)))
         (body (outlook::mail-body mail)))
    (is eq :rtf (outlook::mail-body-kind mail))
    (is string= (format nil "Kea été~%€done") body))
  (is string=
      "{\\rtf1\\ansi"
      (tuatara.text:octets-latin-1 (outlook::lzfu-decompress (lzfu-test-octets (list '(0 11)))))))

(define-test (outlook crafted-compound-files)
  (let* ((good (msg-test-octets))
         (dir (tuatara.io:octets-integer good 48 4))
         (fat (* 512 (1+ (tuatara.io:octets-integer good 76 4)))))
    (flet ((refused (octets words)
             (let ((start (get-internal-real-time)))
                 (fail (outlook::msg-read octets))
                 (let ((said (handler-case (progn (outlook::msg-read octets) nil) (error (e) (princ-to-string e)))))
                 (true (and said (search words said)) "~s said ~s" words said))
               (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))))))
      ;; a FAT whose chain comes back on itself
      (let ((loop (copy-seq good)))
        (tuatara.io:octets-put-integer loop dir 4 :at (+ fat (* 4 dir)))
        (refused loop "loops"))
      ;; a stream saying it holds more than its chain does: the subject's
        (let* ((cfb (outlook::cfb-read good))
               (subject (find "__substg1.0_0037001F" (outlook::cfb-entries cfb)
                              :key (lambda (e) (and e (outlook::cfb-entry-name e))) :test #'equal))
             (big (copy-seq good))
               (at (+ (* 512 (1+ dir)) (* 128 (outlook::cfb-entry-index subject)) 120)))
        (tuatara.io:octets-put-integer big 4000 4 :at at)
        (refused big "more than its"))
      ;; not a compound file, and one cut short
      (refused (make-array 600 :element-type '(unsigned-byte 8) :initial-element 0) "Not a compound file")
      (refused (subseq good 0 1024) "compound file")))
  ;; items attached within items, deeper than *outlook-nesting-limit*: read to it, the rest listed as not read
  (let* ((mail (outlook::msg-read (msg-test-octets :inner-depth 20)))
         (deepest mail))
    (loop for attachment = (car (last (outlook::mail-attachments deepest)))
          while (and attachment (outlook::mail-attachment-message attachment))
          do (setf deepest (outlook::mail-attachment-message attachment)))
    (has (outlook::mail-attachment-why (car (last (outlook::mail-attachments deepest)))) "nested too deep"))
  ;; an attached item made to hold the item it is in: read once, never round and round
  (let* ((good (msg-test-octets))
         (cfb (outlook::cfb-read good))
         (entries (outlook::cfb-entries cfb))
         (inner (find "__substg1.0_3701000D"
                      entries
                      :key (lambda (e) (and e (outlook::cfb-entry-name e)))
                      :test #'equal))
         (circle (copy-seq good))
         (at (+ (* 512 (1+ (tuatara.io:octets-integer good 48 4))) (* 128 (outlook::cfb-entry-index inner)) 76))
         (start (get-internal-real-time)))
    (tuatara.io:octets-put-integer circle (outlook::cfb-entry-child (aref entries 0)) 4 :at at)
    (let ((mail (outlook::msg-read circle)))
      (is string= "Kea survey results" (outlook::mail-subject mail)))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))))

;;; Internet mail

(defparameter *eml-test-text*
  (format nil "From: =?UTF-8?B?S8SBcmVhIEJpcmQ=?= <karearea@example.org>~@
To: Ruru <ruru@example.org>~@
Subject: =?iso-8859-1?Q?Caf=E9?= =?UTF-8?Q?_survey?=~@
Date: Tue, 6 Oct 2026 20:15:00 +1300~@
Message-ID: <eml-1@example.org>~@
Received: from a.example.org by b.example.org; Tue, 6 Oct 2026 07:16:00 -0000~@
X-Mailer: Feather 1.0~@
MIME-Version: 1.0~@
Content-Type: multipart/mixed; boundary=\"outer\"~@
~@
Preamble, not shown.~@
--outer~@
Content-Type: multipart/alternative; boundary=\"inner\"~@
~@
--inner~@
Content-Type: text/plain; charset=utf-8~@
Content-Transfer-Encoding: quoted-printable~@
~@
Caf=C3=A9 count: twelve =~@
kea.~@
--inner~@
Content-Type: text/html; charset=utf-8~@
Content-Transfer-Encoding: base64~@
~@
PHA+Q2Fmw6k8L3A+~@
--inner--~@
--outer~@
Content-Type: image/png~@
Content-Disposition: attachment; filename*=utf-8''r%C3%A9sum%C3%A9.png~@
Content-Transfer-Encoding: base64~@
~@
~a~@
--outer~@
Content-Type: message/rfc822~@
~@
From: Inner <inner@example.org>~@
Subject: Forwarded count~@
~@
Inner text.~@
--outer--~@
Epilogue.~%"
          (let ((o (coerce *png-test-octets* '(simple-array (unsigned-byte 8) (*)))))
            ;; in lines of 60, as mail writes it
            (let ((all (tuatara.io:base64-string o)))
              (format nil
                      "~{~a~^~%~}"
                      (loop for at from 0 below (length all) by 60 collect (subseq all at (min (length all) (+ at 60))))))))
  "A saved mail for the tests: encoded words, quoted-printable, base64, parts within parts, an attached picture and message.")

(define-test (outlook internet-mail)
  (let* ((mail (outlook::eml-read (sb-ext:string-to-octets *eml-test-text* :external-format :utf-8)))
         (text (mail-test-text mail)))
    (is string= "Kārea Bird <karearea@example.org>" (outlook::mail-from mail))
    (is string= "Café survey" (outlook::mail-subject mail))
    (is = (encode-universal-time 0 15 7 6 10 2026 0) (outlook::mail-sent mail))
    (is = (encode-universal-time 0 16 7 6 10 2026 0) (outlook::mail-received mail))
    (is eq :plain (outlook::mail-body-kind mail))
    (is string= (format nil "Café count: twelve kea.") (string-right-trim '(#\Newline) (outlook::mail-body mail)))
    ;; its HTML kept beside the plain text shown, for drawing it as it looks
    (is string= "<p>Café</p>" (funcall (outlook::mail-html mail)))
    (has text "X-Mailer: Feather 1.0")
    (has text "Content-Type: multipart/mixed")
    (false (search "Preamble" (outlook::mail-body mail)))
    (let ((attachments (outlook::mail-attachments mail)))
      (is = 2 (length attachments))
      (is string= "résumé.png" (outlook::mail-attachment-name (first attachments)))
      ;; an attachment is decoded only when its contents are asked for; its size is told before
      (false (outlook::mail-attachment-octets (first attachments)))
      (is = (length *png-test-octets*) (outlook::mail-attachment-size (first attachments)))
      (false (nth-value 1 (outlook::mail-attachment-words (first attachments))))
      (false (outlook::mail-attachment-octets (first attachments)))
      (is equalp *png-test-octets* (outlook::mail-attachment-contents (first attachments)))
      (is string= "Forwarded count" (outlook::mail-attachment-name (second attachments)))
      (is string=
          "Inner text."
          (string-right-trim '(#\Newline)
                             (outlook::mail-body (outlook::mail-attachment-message (second attachments)))))))
  ;; quoted-printable and base64 on their own
  (is string=
      "a=b é"
      (tuatara.text:charset-text (outlook::mime-quoted-printable-octets (sb-ext:string-to-octets (format nil "a=3Db =~%=C3=A9") :external-format :latin-1)) "utf-8"))
  (is string=
      "Man"
      (tuatara.text:octets-latin-1 (tuatara.io:base64-octets (sb-ext:string-to-octets (format nil "TW~%Fu")
                                                                                      :external-format
                                                                                      :latin-1))))
  (is string=
      "Ma"
      (tuatara.text:octets-latin-1 (tuatara.io:base64-octets (sb-ext:string-to-octets "TWE=" :external-format :latin-1))))
  ;; a boundary that never comes: the body is the rest, and the message says so
  (let ((mail (outlook::eml-read (sb-ext:string-to-octets (format nil
                                                                  "Subject: broken~%Content-Type: multipart/mixed; boundary=\"gone\"~%~%Only text.~%")
                                                          :external-format
                                                          :utf-8))))
    (is string= "broken" (outlook::mail-subject mail))
    (true (some (lambda (note) (search "boundary" note)) (outlook::mail-notes mail))))
  ;; a closing line that never comes: the last part runs to the end
  (let ((mail (outlook::eml-read (sb-ext:string-to-octets (format nil
                                                                  "Content-Type: multipart/mixed; boundary=b~%~%--b~%Content-Type: text/plain~%~%cut short~%")
                                                          :external-format
                                                          :utf-8))))
    (has (outlook::mail-body mail) "cut short"))
  ;; a date of every kind, or none
  (is = (encode-universal-time 0 0 12 1 1 2026 0) (outlook::mail-date-time "1 Jan 26 12:00 GMT"))
  (false (outlook::mail-date-time "yesterday"))
  ;; headers in charsets of their own, and an unknown one read as the editor reads files
  (is string= "€" (outlook::mime-decode-words "=?windows-1252?Q?=80?="))
  (is string= "plain" (outlook::mime-decode-words "plain"))
  ;; a header folded over very many short lines, and a body of very many link
  ;; ends, are read at once rather than hanging the reader
  (let* ((lines 200000)
         (octets (sb-ext:string-to-octets
                  (with-output-to-string (out)
                    (format out "Subject: a")
                    (dotimes (i lines) (format out "~% b"))
                    (format out "~%~%Body.~%"))
                  :external-format :latin-1))
         (start (get-internal-real-time))
         (mail (outlook::eml-read octets)))
    (is = (+ 1 (* 2 lines)) (length (outlook::mail-subject mail)))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second))))
  (let* ((html (with-output-to-string (out) (dotimes (i 200000) (write-string "</a>x" out))))
         (start (get-internal-real-time))
         (text (outlook::html-text html)))
    (is = 200000 (length text))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second))))
  (is string= "Kea <https://example.org/>" (outlook::html-text "<a href=\"https://example.org/\">Kea</a>"))
  (is string= "https://example.org/" (outlook::html-text "<a href=\"https://example.org/\">https://example.org/</a>")))

;;; Seen and opened in the editor

(defun outlook-busy-p ()
  "True while a message or a mailbox is being read, or a message drawn as it looks."
  (or (some (lambda (job)
              (or (outlook::mail-read-job-p job) (outlook::mailbox-job-p job) (outlook::mail-pages-job-p job)))
            tuatara.runtime:*jobs*)
      (some (lambda (b) (let ((view (tuatara.defaults::pdf-view-of b)))
                          (and view
                               (or (tuatara.defaults::pdf-view-job view) (tuatara.defaults::pdf-view-text-job view)))))
            tuatara.defaults::*pdf-views*)))

(defun outlook-session (path steps &key (width 120) (height 40) cell-pixels)
  "Visit PATH with C-x C-f in an editor session, then do each of STEPS (keys,
or a function called there) once what is read in the background is done.
CELL-PIXELS has the frontend show pictures. Returns the frontend and the editor."
  (let ((f (tuatara.frontend.fake:make-fake-frontend :width width :height height :cell-pixels cell-pixels))
        (start (get-internal-real-time)))
    (let ((scratch (tuatara.buffer:find-buffer "*scratch*")))
      (when scratch (tuatara.buffer:kill-buffer scratch)))
    (tuatara.buffer:make-buffer "*scratch*" :text "")
    (tuatara.frontend.fake:fake-queue-keys f (format nil "C-x C-f C-a C-k ~a RET" (rimu-typed path)))
    (labels ((check ()
               (if (and (outlook-busy-p) (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))
                   (progn (sleep 0.02) (tuatara.frontend.fake:fake-queue-action f #'check))
                   (when steps
                     (let ((step (pop steps)))
                       (if (functionp step)
                           (tuatara.frontend.fake:fake-queue-action f step)
                           (tuatara.frontend.fake:fake-queue-keys f step)))
                     (tuatara.frontend.fake:fake-queue-action f #'check)))))
      (tuatara.frontend.fake:fake-queue-action f #'check))
    (values f (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                            :buffer (tuatara.buffer:find-buffer "*scratch*")))))

(defun to-attachment (n)
  "A step for OUTLOOK-SESSION: point to the Nth attachment's button of the
message shown."
  (lambda ()
    (let ((place (nth n (tuatara.buffer:buffer-local (tuatara.command:current-buffer) 'outlook::mail-attachment-places))))
      (when place (setf (tuatara.buffer:buffer-point (tuatara.command:current-buffer)) (first place))))))

(define-test (outlook item-opened-attachments-safely)
  (with-test-directory (dir)
    (let* ((path (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets)))
           (before (tuatara.io:file-octets path))
           (stamp (tuatara.io:path-stamp path)))
      ;; visited: a message, read only
      (multiple-value-bind (f editor) (outlook-session path '())
        (declare (ignore f))
        (let ((buffer (session-buffer editor)))
          (is string= "survey.msg" (tuatara.buffer:buffer-name buffer))
            (true (tuatara.command::buffer-mode-named-p buffer 'outlook::mail-view-mode))
          (true (tuatara.buffer:buffer-local buffer :read-only))
          (has (session-text editor) "Kea survey results")
          (tuatara.buffer:kill-buffer buffer)))
      ;; RET on the picture: its private copy shown as rimu shows it, a picture drawn in the sandbox
      (when (and (eq :ready (tuatara.io:sandbox-status)) (tuatara.defaults::find-image-decoder))
        (multiple-value-bind (f editor) (outlook-session path (list (to-attachment 0) "RET") :cell-pixels '(10 20))
          (declare (ignore f))
          (let* ((view (tuatara.defaults::image-view-of (session-buffer editor)))
                 (copy (and view (tuatara.defaults::image-view-path view))))
            (true view)
            (is string= "kea.png" (and copy (tuatara.io:native-file-name copy)))
            (tuatara.buffer:kill-buffer (session-buffer editor))
            (when (and copy (search "/opened/" copy)) (tuatara.io:delete-tree (tuatara.io:parent-path copy))))))
      ;; with no view for it, its private copy opened safely; on the item attached, a message
      (multiple-value-bind (f editor) (let ((outlook::*mail-attachment-views* '()))
                                        (outlook-session path (list (to-attachment 0) "RET")))
        (declare (ignore f))
        (let* ((shown (session-buffer editor))
               (copy (tuatara.io:native (tuatara.buffer:buffer-file shown))))
          (true (tuatara.io:safe-visit-p shown))
          (is string= "kea.png" (tuatara.io:native-file-name copy))
          (true (search "/opened/" copy))
          (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp copy))))
          (is equalp *png-test-octets* (tuatara.io:file-octets copy))
          (tuatara.buffer:kill-buffer shown)
          (when (search "/opened/" copy) (tuatara.io:delete-tree (tuatara.io:parent-path copy)))))
      (multiple-value-bind (f editor) (outlook-session path (list (to-attachment 1) "RET"))
        (declare (ignore f))
        (has (session-text editor) "Inner body 1")
          (true (tuatara.command::buffer-mode-named-p (session-buffer editor) 'outlook::mail-view-mode)))
      ;; T: the file itself, as text
      (multiple-value-bind (f editor) (outlook-session path (list "T"))
        (declare (ignore f))
        (is equal (tuatara.io:native path) (tuatara.io:native (tuatara.buffer:buffer-file (session-buffer editor)))))
      (dolist (b (copy-list tuatara.buffer:*buffers*))
          (when (tuatara.command::buffer-mode-named-p b 'outlook::mail-view-mode) (tuatara.buffer:kill-buffer b)))
      ;; the item itself never touched
      (is equalp before (tuatara.io:file-octets path))
      (is = (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime (tuatara.io:path-stamp path)))))
  ;; names an attachment gives made a name of a file alone
  (is string= "passwd" (tuatara.defaults::attachment-file-name "../../etc/passwd"))
  (is string= "b.exe" (tuatara.defaults::attachment-file-name "a\\b.exe"))
  (is string= "attachment" (tuatara.defaults::attachment-file-name ".."))
  (is string= "attachment" (tuatara.defaults::attachment-file-name nil)))

(define-test (outlook saved-mail-opened)
  (with-test-directory (dir)
    (let ((path (write-test-octets (merge-pathnames "count.eml" dir)
                                   (sb-ext:string-to-octets *eml-test-text* :external-format :utf-8))))
      (multiple-value-bind (f editor) (outlook-session path '())
        (declare (ignore f))
        (has (session-text editor) "Café survey")
        (has (session-text editor) "résumé.png")
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; T shows its text, as any file is opened
      (multiple-value-bind (f editor) (outlook-session path (list "T"))
        (declare (ignore f))
        (has (session-text editor) "Content-Transfer-Encoding: quoted-printable")
        (dolist (b (copy-list tuatara.buffer:*buffers*))
            (when (or (tuatara.command::buffer-mode-named-p b 'outlook::mail-view-mode) (eq b (session-buffer editor)))
            (tuatara.buffer:kill-buffer b)))))))

;;; As it looks

(define-test (outlook html-made-ready)
  ;; a picture shown by its Content-ID written beside the HTML and pointed to; one of no attachment left as it is
  (with-test-directory (dir)
    (let* ((directory (namestring dir))
           (picture (outlook::make-mail-attachment :name "kea.png"
                                                   :octets *png-test-octets*
                                                   :content-id "<kea%401@example.org>"))
           (other (outlook::make-mail-attachment :name "notes.txt" :octets (tuatara.text:string-octets "notes")))
           (html "<img src=\"cid:kea@1@example.org\"><img src='CID:kea%401@example.org'><img src=\"cid:gone\"><img src=\"https://example.org/track.png\"><div style=\"background:url( 'http://example.org/b.gif')\"></div><link rel=stylesheet href=\"//example.org/s.css\"><a href=\"https://example.org/\">a link</a>"))
      (multiple-value-bind (ready fetching) (outlook::mail-page-html html (list other picture) directory)
        (has ready "<img src=\"part-1.png\"><img src='part-1.png'><img src=\"cid:gone\">")
        ;; the picture, the background and the style would be fetched; a link is never followed
        (is = 3 fetching)
        (is equal '("part-1.png") (mapcar #'first (tuatara.io:directory-entries directory)))
        (is equalp *png-test-octets* (tuatara.io:file-octets (merge-pathnames "part-1.png" dir))))
      ;; a name an attachment gives is never used but for a plain ending
      (let ((odd (outlook::make-mail-attachment :name "x.p/n g" :octets *png-test-octets* :content-id "odd")))
        (has (outlook::mail-page-html "<img src=cid:odd>" (list odd) directory) "src=part-1>")))))

(define-test (outlook page-head)
  ;; the headers drawn at the start of the body, written as text, never as markup
  (let ((mail (outlook::make-mail :from "Kea <kea@example.org>" :subject "Tui & </td><script>x</script>"
                                  :attachments (list (outlook::make-mail-attachment :name "kea.png")
                                                     (outlook::make-mail-attachment :name "logo.png" :hidden t)))))
    (flet ((head-at (html) (search "<table" (outlook::mail-page-with-headers html mail))))
      (is = 20 (head-at "<html><body class=a><p>x</p></body></html>"))
      (is = 19 (head-at "<html><head></head><p>x</p></html>"))
      (is = 21 (head-at "<!DOCTYPE html><html><p>x</p></html>"))
      (is = 0 (head-at "<p>x</p>")))
    (let ((head (outlook::mail-page-head mail)))
      (has head "Kea &lt;kea@example.org&gt;")
      (has head "Tui &amp; &lt;/td&gt;&lt;script&gt;")
      (false (search "<script" head))
      (has head "kea.png")
      (false (search "logo.png" head)))
    ;; the internet headers on a page of their own, before the last end of the body
    (false (outlook::mail-page-foot mail))
    (setf (outlook::mail-headers mail)
          (format nil "Received: from <mx>~c~cX-Note: a & b~c~c" #\Return #\Newline #\Return #\Newline))
    (let ((foot (outlook::mail-page-foot mail)))
      (has foot "Received: from &lt;mx&gt;
X-Note: a &amp; b</pre>")
      (flet ((foot-before (html tail) (let ((all (outlook::mail-page-with-headers html mail)))
                                        (is = (- (length all) (length tail)) (+ (search foot all) (length foot))))))
        (foot-before "<html><body><p>x</p></body><!-- </body> --></BODY></html>" "</BODY></html>")
        (foot-before "<html><p>x</p></html>" "</html>")
        (foot-before "<p>x</p>" "")
        ;; an end of the body before its start: both after the start
        (foot-before "</body><body><p>x</p>" "<p>x</p>")))))

(defun mail-pages-drawn-p ()
  "True when messages can be drawn as they look here: the sandbox ready,
WeasyPrint installed in it and a program drawing PDFs."
  (and (eq :ready (tuatara.io:sandbox-status))
       (tuatara.io:sandbox-find-program outlook::*outlook-page-program*)
       (tuatara.defaults::find-pdf-decoder)))

(define-test (outlook item-drawn-as-it-looks)
  (with-test-directory (dir)
    (let* ((html (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets :body :html)))
           (plain (write-test-octets (merge-pathnames "plain.msg" dir) (msg-test-octets)))
           (before (tuatara.io:file-octets html)))
      ;; plain text alone looks as it is shown
      (multiple-value-bind (f editor) (outlook-session plain (list "P"))
        (true (tuatara.frontend.fake:fake-frame-showing f "Its text is plain"))
        (tuatara.buffer:kill-buffer (session-buffer editor)))
      ;; where WeasyPrint is not, it says what to install
      (let ((outlook::*outlook-page-program* "no-such-weasyprint"))
        (multiple-value-bind (f editor) (outlook-session html (list "P"))
          (true (tuatara.frontend.fake:fake-frame-showing f "no-such-weasyprint, which is not installed"))
          (tuatara.buffer:kill-buffer (session-buffer editor))))
      (if (not (mail-pages-drawn-p))
          (skip "WeasyPrint, the sandbox or a program for PDFs is not here" (true t))
          ;; P: its HTML drawn as pages, shut in, the script not run and nothing fetched; T back to the message
          (let ((pages nil) (back nil))
            (multiple-value-bind (f editor)
                (outlook-session html (list "P" (lambda () (setf pages (tuatara:current-buffer))) "T"
                                            (lambda () (setf back (tuatara:current-buffer)))))
              (declare (ignore f))
              (is string= "survey.msg (as it looks)" (tuatara.buffer:buffer-name pages))
              (true (tuatara.command::buffer-mode-named-p pages 'tuatara.defaults::pdf-view-mode))
              (let ((text (tuatara.buffer:buffer-string pages)))
                (has text "Kea & kaka seen")
                ;; its headers above it, as a mail program prints it
                (has text "Kea survey results")
                (has text "Ruru Canterbury")
                (has text "X-Origin: tested")
                (false (search "alert" text)))
              (let ((view (tuatara.defaults::pdf-view-of pages)))
                ;; the message, then its internet headers on a page of their own
                (is = 2 (tuatara.defaults::pdf-view-pages view))
                ;; fitted to the window's width, to be read
                (is eq :width (tuatara.defaults::pdf-view-zoom view))
                (has (tuatara.defaults::pdf-view-words view) "1 thing from the internet not fetched"))
              (true (tuatara.command::buffer-mode-named-p back 'outlook::mail-view-mode))
              ;; its HTML written for you alone, beside nothing of yours
              (let ((written (tuatara.defaults::pdf-view-path (tuatara.defaults::pdf-view-of pages))))
                (true (search "/mail-pages/" written))
                (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp written))))
                (tuatara.io:delete-tree (tuatara.io:parent-path written)))
              (dolist (b (list pages (session-buffer editor) back))
                (when (tuatara.buffer:live-buffer-p b) (tuatara.buffer:kill-buffer b))))))
      (is equalp before (tuatara.io:file-octets html)))))

;;; Mailboxes

(defun put-test-file (path text)
  "Write TEXT at PATH, its directories made."
  (ensure-directories-exist path)
  (write-test-octets path (sb-ext:string-to-octets text :external-format :utf-8)))

(define-test (outlook mailbox-keys-as-every-list)
  ;; a mailbox opens, steps and marks with the keys of every list, by Tuatara's own commands
  (let ((map (tuatara.modes:mode-keymap 'outlook::mailbox-mode)))
    (flet ((bound (keys) (tuatara.command:lookup-key map (tuatara.frontend:parse-key-sequence keys))))
      (is eq 'tuatara.defaults::list-visit (bound "RET"))
      (is eq 'tuatara.defaults::list-visit (bound "l"))
      (is eq 'tuatara.defaults::list-visit-other (bound "o"))
      (is eq 'tuatara.defaults::list-next-entry (bound "n"))
      (is eq 'tuatara.defaults::list-previous-entry (bound "p"))
      (is eq 'tuatara.defaults::list-mark-entry (bound "m")))))

(define-test (outlook mailbox-listed)
  ;; what readpst writes, made by hand: readpst itself is run only where it is installed
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "out/" dir))
           (pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                   (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
             (job (outlook::make-mailbox-job buffer pst (namestring out))))
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/1.eml" out)
                     (format nil
                             "From: Kea <kea@example.org>~%Subject: First~%Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%one~%"))
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/2.eml" out) *eml-test-text*)
      (put-test-file (merge-pathnames "Outlook Data File/Inbox/10.eml" out) (format nil "Subject: Tenth~%~%ten~%"))
      (put-test-file (merge-pathnames "Outlook Data File/Contacts/1.vcf" out)
                     (format nil "BEGIN:VCARD~%FN:Kea~%END:VCARD~%"))
      (sb-posix:symlink "/etc/passwd" (namestring (merge-pathnames "Outlook Data File/Inbox/3.eml" out)))
      (is eq :pst (nth-value 1 (tuatara.io:octets-kind (tuatara.io:file-head pst))))
        (outlook::mailbox-list job)
        (let ((folders (outlook::mailbox-job-folders job)))
        (is equal '("Outlook Data File/Contacts" "Outlook Data File/Inbox") (mapcar #'car folders))
        ;; in the order of their numbers, the link passed over
          (is equal '("First" "Café survey" "Tenth") (mapcar #'outlook::mailbox-item-subject (cdr (second folders))))
          (is equal '(0 2 0) (mapcar #'outlook::mailbox-item-attachments (cdr (second folders))))
          (is string= "1.vcf" (outlook::mailbox-item-subject (first (cdr (first folders))))))
        (tuatara.modes:set-major-mode buffer 'outlook::mailbox-mode)
        (outlook::mailbox-render buffer job)
      (let ((text (tuatara.buffer:buffer-string buffer)))
        (has text "Outlook mailbox archive.pst")
        (has text "4 items in 2 folders")
        (has text "▾ Outlook Data File/Inbox (3)")
        (has text "2026-10-06")
        (has text "Café survey"))
      ;; each line its item
      (let ((item (tuatara.defaults::entry-member (tuatara.defaults::list-entry-at buffer
                                                                                   (tuatara.buffer:buffer-point buffer)))))
          (true (outlook::mailbox-item-p item)))
      (tuatara.buffer:kill-buffer buffer))))

;;; A mailbox's list narrowed by its messages' headers and contents

(defun mailbox-search-test-messages ()
  "The messages the search of a mailbox's list is tried on, each (NAME .
TEXT) as readpst would write it: a header not shown in the list, bodies
quoted-printable and base64, an attachment's name, and one larger than the
search looks through."
  (list (cons "Inbox/1.eml"
              (format nil "From: Kea <kea@example.org>~%Subject: First~%X-Case-Number: walrus-7~%~%nothing here~%"))
        (cons "Inbox/2.eml"
              (format nil "From: Ruru <ruru@example.org>~%Subject: Second~%Content-Type: text/plain; charset=utf-8~%~
                           Content-Transfer-Encoding: quoted-printable~%~%Caf=C3=A9 and a gan=~%net seen~%"))
        (cons "Inbox/3.eml"
              (format nil "From: Tui <tui@example.org>~%Subject: Third~%Content-Type: text/plain~%~
                           Content-Transfer-Encoding: base64~%~%dGhlIGFsYmF0cm9zcyBmbGllcw==~%"))
        (cons "Inbox/4.eml"
              (format nil "From: Weka <weka@example.org>~%Subject: Fourth~%MIME-Version: 1.0~%~
                           Content-Type: multipart/mixed; boundary=\"b\"~%~%--b~%Content-Type: text/plain~%~%~
                           see attached~%--b~%Content-Type: application/pdf; name=\"invoice-2026.pdf\"~%~
                           Content-Transfer-Encoding: base64~%~%JVBERi0xLjQK~%--b--~%"))
        (cons "Inbox/5.eml"
              (format nil "From: Hoiho <hoiho@example.org>~%Subject: Fifth~%~%~a petrel~%"
                      (make-string 400 :initial-element #\x)))))

(defun mailbox-test-list (dir files &key (sum "0f1e2d3c") (search-limit tuatara-user:*outlook-search-text-limit*))
  "The list of a mailbox whose items readpst would have written as FILES, each
(NAME . TEXT), NAME from the mailbox's top folder and TEXT a string or octets,
into DIR: read for it, its
messages' text written as the listing job writes it, searched as far as
SEARCH-LIMIT, its sum taken to be SUM (NIL for none made). Returns the list
and the job."
  (ensure-directories-exist dir)
  (let* ((out (merge-pathnames "out/" dir))
         (texts (merge-pathnames "text/" dir))
         (pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                 (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
         (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
         (job (outlook::make-mailbox-job buffer pst (namestring out) (namestring texts))))
    (loop for (name . text) in files
          for path = (merge-pathnames (concatenate 'string "Outlook Data File/" name) out)
          do (if (stringp text)
                 (put-test-file path text)
                 (write-test-octets (ensure-directories-exist path) text)))
    (ensure-directories-exist texts)
    (setf (outlook::mailbox-job-sum job) sum)
    (let ((tuatara-user:*outlook-search-text-limit* search-limit))
      (outlook::mailbox-list job))
    (tuatara.modes:set-major-mode buffer 'outlook::mailbox-mode)
    (setf (tuatara.buffer:buffer-local buffer 'outlook::mailbox-path) pst)
    (outlook::mailbox-render buffer job)
    (values buffer job)))

(defun mailbox-search-test-list (dir)
  "The list of the mailbox MAILBOX-SEARCH-TEST-MESSAGES writes in DIR, read
for it and its messages' text written as the listing job writes it, with
*OUTLOOK-SEARCH-TEXT-LIMIT* small enough that the fifth is too large."
  (values (mailbox-test-list dir (mailbox-search-test-messages) :search-limit 300)))

(defun mailbox-subjects (buffer)
  "The subjects of the messages BUFFER, a mailbox's list, shows."
  (loop for line in (uiop:split-string (tuatara.buffer:buffer-string buffer) :separator '(#\Newline))
        for found = (find-if (lambda (subject) (search subject line)) '("First" "Second" "Third" "Fourth" "Fifth"))
        when (and found (not (search "iltered by" line))) collect found))

(defun wait-for-mailbox-search ()
  "Wait, a few seconds at most, till no list is being filtered in the background."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job)
                             (and (tuatara.defaults::search-filter-job-p job) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(defun mailbox-search-session (list keys)
  "Type KEYS, after showing LIST, a mailbox's list, in an editor session:
:SEARCHED among them waits for the search in the background and lets the
editor take it in, :LOOK keeps the subjects shown there. Returns the editor
and the subjects kept."
  (let ((seen nil))
    (multiple-value-bind (f editor)
        (tuatara.tests::run-keys
         (list* (lambda () (tuatara:show-buffer list))
                (loop for key in keys
                      append (case key
                               (:searched (list #'wait-for-mailbox-search :tick))
                               (:look (list (lambda () (setf seen (mailbox-subjects list)))))
                               (t (list key)))))
         :width 140 :height 30)
      (declare (ignore f))
      (values editor seen))))

(define-test (outlook mailbox-searched)
  (with-test-directory (dir)
    (let ((list (mailbox-search-test-list dir)))
      (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list))
      ;; the text written for each message: every header, the text parts decoded, attachments by name
      (let ((texts (loop for entry across (coerce (tuatara.buffer:buffer-local list 'tuatara.defaults::list-entries) 'vector)
                         for item = (tuatara.defaults::entry-member entry)
                         when (outlook::mailbox-item-p item) collect (outlook::mailbox-item-search-text item))))
        (has (first texts) "x-case-number: walrus-7")
        (has (second texts) "Café and a gannet seen")
        (has (third texts) "the albatross flies")
        (has (fourth texts) "invoice-2026.pdf")
        (false (search "PDF-1" (fourth texts))))
      (has (tuatara.buffer:buffer-string list) "1 message too large to be searched whole")
      (flet ((narrowed (pattern)
               (nth-value 1
                          (mailbox-search-session list
                                                  (list (format nil "C-s ~a" (tuatara.tests::rimu-typed pattern))
                                                        :searched
                                                        :look)))))
        ;; a header the list does not show
        (is equal '("First") (narrowed "walrus-7"))
        ;; quoted-printable, its soft line break joined and its UTF-8 read; base64
        (is equal '("Second") (narrowed "gannet"))
        (is equal '("Third") (narrowed "albatross"))
        ;; an attachment's name, never its contents
        (is equal '("Fourth") (narrowed "invoice-20"))
        (is equal '() (narrowed "PDF-1"))
        ;; past what the search looks through in a message too large
        (is equal '() (narrowed "petrel"))
        ;; headers matched by a regular expression, case ignored
        (is equal '("Second" "Third") (narrowed "^from: (ruru|tui)")))
      ;; C-g brings the whole list back
      (multiple-value-bind (editor seen) (mailbox-search-session list '("C-s w a l r u s" :searched :look "C-g"))
        (declare (ignore editor))
        (is equal '("First") seen)
        (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list)))
      ;; kept with RET, a second search narrows what the first left; then all taken away
      (mailbox-search-session list '("C-s e e n RET" :searched "C-s a" :searched "RET"))
      (is equal '("Second") (mailbox-subjects list))
      (is equal '("een" "a") (mapcar #'tuatara:search-filter-string (tuatara:search-filters list)))
      (mailbox-search-session list '("C-x / r"))
      (is equal '("First" "Second" "Third" "Fourth" "Fifth") (mailbox-subjects list))
      ;; RET opens the message on the line of the narrowed list, the filter's matches lit in it
      (multiple-value-bind (editor) (mailbox-search-session
                                     list (list "C-s a l b a" :searched "RET"
                                                (lambda ()
                                                  (setf (tuatara.buffer:buffer-point list)
                                                        (tuatara.buffer:buffer-goto-line
                                                         list (position-if (lambda (line) (search "Third" line))
                                                                           (uiop:split-string (tuatara.buffer:buffer-string list)
                                                                                              :separator '(#\Newline))))))
                                                "RET"))
        (let ((shown (tuatara.display:window-buffer (tuatara.command:editor-window editor))))
          (is string= "Third" (tuatara.buffer:buffer-name shown))
          (is eq shown (cdr tuatara.defaults::*search-lit*))
          (setf tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
          (tuatara.buffer:kill-buffer shown)))
      (tuatara.buffer:kill-buffer list))))

;;; Labels

(defun mailbox-test-subject-line (list subject)
  "The line of LIST, a mailbox's list, that shows SUBJECT, or NIL."
  (find-if (lambda (line) (search subject line))
           (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline))))

(defun mailbox-test-go-to (list subject)
  "A step for RUN-KEYS: point on the line of LIST that shows SUBJECT."
  (lambda ()
    (let ((lines (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline))))
      (setf (tuatara.buffer:buffer-point list)
            (tuatara.buffer:buffer-goto-line list (position-if (lambda (line) (search subject line)) lines))))))

(defun wait-for-mailbox-labels ()
  "Wait, a few seconds at most, till no mailbox's labels are being written."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (or (outlook::mailbox-labels-job-p job) (outlook::mailbox-labels-manager-job-p job))
                                              (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(defparameter *mailbox-label-test-messages*
  (list (cons "Inbox/1.eml" (format nil "From: Kea <kea@example.org>~%Subject: First~%Message-ID: <one@example.org>~%~%a~%"))
        (cons "Inbox/2.eml" (format nil "From: Ruru <ruru@example.org>~%Subject: Second~%~%b~%"))
        (cons "Inbox/3.eml" (format nil "From: Tui <tui@example.org>~%Subject: Third~%Message-ID: <three@example.org>~%~%c~%")))
  "The messages labelled in the tests: two with a Message-ID, one known by its headers alone.")

(define-test (outlook mailbox-labels)
  (with-test-directory (dir)
    (let ((sum (format nil "test~36r" (random (expt 2 40)))))
      (multiple-value-bind (list job) (mailbox-test-list dir *mailbox-label-test-messages* :sum sum)
        (let ((items (cdr (first (outlook::mailbox-job-folders job)))))
          ;; known by the folder, the Message-ID (empty for none) and the sum of the headers
          (true (uiop:string-prefix-p (format nil "Outlook Data File/Inbox~c<one@example.org>~c" #\Tab #\Tab)
                                      (outlook::mailbox-item-identity (first items))))
          (is = (+ (length (format nil "Outlook Data File/Inbox~c~c" #\Tab #\Tab)) 64)
              (length (outlook::mailbox-item-identity (second items)))))
        ;; no labels yet: no column for them
        (false (search "Labels" (mailbox-test-subject-line list "Subject")))
        ;; + on the line, two labels typed, then the empty choice ends the question; m marks two, + labels both
        (tuatara.tests::run-keys
         (list (lambda () (tuatara:show-buffer list))
               (mailbox-test-go-to list "First") "+ u r g e n t RET k e a SPC c a s e RET RET"
               (mailbox-test-go-to list "Second") "m m + u r g RET RET" #'wait-for-mailbox-labels :tick)
         :width 140 :height 30)
        (has (mailbox-test-subject-line list "First") "urgent, kea case")
        (has (mailbox-test-subject-line list "Second") "urgent")
        (has (mailbox-test-subject-line list "Third") "urgent")
        (has (mailbox-test-subject-line list "Subject") "Labels")
        ;; kept in the state directory by the mailbox's sum, nothing written beside the mailbox
        (let ((file (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
          (has (uiop:read-file-string file) (format nil "+~curgent~cOutlook Data File/Inbox~c<one@example.org>~c"
                                                    #\Tab #\Tab #\Tab #\Tab))
          (is equal '("archive.pst" "out" "text")
              (sort (mapcar #'first (tuatara.io:directory-entries (namestring dir))) #'string<)))
        (is equal '("kea case" "urgent") (outlook::mailbox-labels-used job))
        ;; - takes one off the line's message, chosen from those it carries
        (tuatara.tests::run-keys
         (list (lambda () (tuatara:show-buffer list))
               "U" (mailbox-test-go-to list "First") "- u r g e n t RET RET" #'wait-for-mailbox-labels :tick)
         :width 140 :height 30)
        (false (search "urgent" (mailbox-test-subject-line list "First")))
        (has (mailbox-test-subject-line list "First") "kea case")
        (tuatara.buffer:kill-buffer list))
      ;; the mailbox read again: its labels back
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "again/" dir) *mailbox-label-test-messages*
                                                         :sum sum)
        (is equal '(("kea case") ("urgent") ("urgent"))
            (mapcar (lambda (item) (outlook::mailbox-item-labels job item)) (cdr (first (outlook::mailbox-job-folders job)))))
        (tuatara.buffer:kill-buffer list))
      ;; no sum made: labels said not to be kept
      (multiple-value-bind (list) (mailbox-test-list (merge-pathnames "nosum/" dir) *mailbox-label-test-messages* :sum nil)
        (tuatara.tests::run-keys (list (lambda () (tuatara:show-buffer list)) (mailbox-test-go-to list "First") "+")
                                 :width 140 :height 30)
        (false (search "Labels" (mailbox-test-subject-line list "Subject")))
        (tuatara.buffer:kill-buffer list)))))

(define-test (outlook mailbox-labels-offered-per-mailbox)
  (with-test-directory (dir)
    (let ((one (format nil "test~36r" (random (expt 2 40)))) (two (format nil "test~36r" (random (expt 2 40)))))
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "one/" dir) *mailbox-label-test-messages*
                                                         :sum one)
        (outlook::mailbox-relabel job (list (first (cdr (first (outlook::mailbox-job-folders job)))))
                                  (lambda (had) (append had '("only here"))))
        (wait-for-mailbox-labels)
        (is equal '("only here") (outlook::mailbox-labels-used job))
        (tuatara.buffer:kill-buffer list))
      ;; another mailbox is offered none of them
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "two/" dir) *mailbox-label-test-messages*
                                                         :sum two)
        (is equal '() (outlook::mailbox-labels-used job))
        (tuatara.buffer:kill-buffer list)))))

(defun mailbox-test-meta (sum)
  "What the journal of the mailbox SUM tells of the mailbox (MAILBOX-JOURNAL-META)."
  (outlook::mailbox-journal-meta
   (outlook::mailbox-journal-lines (tuatara.io:path-join (outlook::mailbox-label-directory)
                                                         (outlook::mailbox-labels-name sum)))))

(define-test (outlook mailbox-labels-journal-tells-its-mailbox)
  (with-test-directory (dir)
    (let ((sum (format nil "test~36r" (random (expt 2 40)))) (before (get-universal-time)))
      ;; no labels, no journal: opening a mailbox writes nothing
      (tuatara.buffer:kill-buffer (mailbox-test-list (merge-pathnames "none/" dir) *mailbox-label-test-messages*
                                                     :sum sum))
      (false (probe-file (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
      ;; labelling tells the file's name, path, size, sum and when it changed
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "one/" dir) *mailbox-label-test-messages*
                                                         :sum sum)
        (outlook::mailbox-relabel job (list (first (cdr (first (outlook::mailbox-job-folders job)))))
                                  (lambda (had) (append had '("kea"))))
        (wait-for-mailbox-labels)
        (destructuring-bind (name path size meta-sum changed) (mailbox-test-meta sum)
          (is string= "archive.pst" name)
          (is string= (namestring (merge-pathnames "one/archive.pst" dir)) path)
          (is = 10 size)
          (is string= sum meta-sum)
          (true (<= before changed (get-universal-time))))
        (tuatara.buffer:kill-buffer list))
      ;; opened from another place: the path is the latest, the change time stays
      (let ((changed (fifth (mailbox-test-meta sum))))
        (tuatara.buffer:kill-buffer (mailbox-test-list (merge-pathnames "two/" dir) *mailbox-label-test-messages*
                                                       :sum sum))
        (is string= (namestring (merge-pathnames "two/archive.pst" dir)) (second (mailbox-test-meta sum)))
        (is = changed (fifth (mailbox-test-meta sum)))))
    ;; a journal from before the line existed still reads, and is given one when opened
    (let* ((sum (format nil "test~36r" (random (expt 2 40))))
           (path (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
      (outlook::mailbox-append-lines path (list (outlook::mailbox-label-line "+" "old" "X")))
      (false (mailbox-test-meta sum))
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "old/" dir) *mailbox-label-test-messages*
                                                         :sum sum)
        (is equal '("old") (gethash "X" (outlook::mailbox-job-labels job)))
        (is string= "archive.pst" (first (mailbox-test-meta sum)))
        (tuatara.buffer:kill-buffer list))
      ;; written again shorter, the line stays
      (outlook::mailbox-append-lines path (loop repeat 600 collect (outlook::mailbox-label-line "+" "old" "X")))
      (let ((meta (mailbox-test-meta sum)))
        (outlook::mailbox-read-labels (outlook::mailbox-labels-name sum))
        (is equal meta (mailbox-test-meta sum))
        (true (< (length (outlook::mailbox-journal-lines path)) 10))))))

(defun mailbox-test-label (job subject &rest labels)
  "Put LABELS on the message of the mailbox JOB read whose subject is SUBJECT."
  (let ((item (find subject (loop for (nil . items) in (outlook::mailbox-job-folders job) append items)
                    :key #'outlook::mailbox-item-subject :test #'equal)))
    (setf (gethash (outlook::mailbox-item-identity item) (outlook::mailbox-job-labels job)) labels)))

(define-test (outlook mailbox-filtered-by-labels)
  (with-test-directory (dir)
    (multiple-value-bind (list job) (mailbox-test-list dir *mailbox-label-test-messages*)
      (mailbox-test-label job "First" "urgent" "kea case")
      (mailbox-test-label job "Third" "seen")
      (outlook::mailbox-render list job)
      (flet ((shown (keys)
               (nth-value 1 (mailbox-search-session list (append (list "C-x / r") keys (list :searched :look))))))
        ;; L: those carrying a label, or any of several; C-u L: those carrying none of them
        (is equal '("First") (shown '("L u r g RET RET")))
        (is equal '("your label: ^(?:urgent)$")
            (mapcar #'tuatara.defaults::search-filter-words (tuatara:search-filters list)))
        (is equal '("First" "Third") (shown '("L u r g RET s e e n RET RET")))
        (is equal '("Second" "Third") (shown '("C-u L u r g RET RET")))
        ;; a label with a regex's characters in it matches only itself
        (mailbox-test-label job "Second" "a.b")
        (outlook::mailbox-render list job)
        (is equal '("Second") (shown '("L a . b RET RET")))
        ;; with the other filters: a search kept, then a label
        (is equal '("Third") (shown '("C-s T h RET" :searched "C-u L u r g RET RET")))
        ;; the field label offered by C-x / f, a pattern of your own
        (is equal '("First") (shown '("C-x / f y o u r SPC l a b e l RET k e a RET"))))
      (tuatara.buffer:kill-buffer list))))

(define-test (outlook mailbox-entries-called-messages)
  ;; what the list's words for marked and acted-on entries call one of a mailbox's
  (with-test-directory (dir)
    (let ((list (mailbox-test-list dir *mailbox-label-test-messages*)))
      (is string= "message" (tuatara:list-entry-noun (tuatara.modes:buffer-major-mode list)))
      (tuatara.buffer:kill-buffer list))))

(define-test (outlook mailbox-labels-kept-apart-and-together)
  (with-test-directory (dir)
    (let ((sum (format nil "test~36r" (random (expt 2 40))))
          (twins (list (cons "Inbox/1.eml" (format nil "Subject: First~%Message-ID: <same@example.org>~%~%a~%"))
                       (cons "Inbox/2.eml" (format nil "Subject: Second~%Message-ID: <same@example.org>~%~%b~%"))
                       (cons "Inbox/3.eml" (format nil "Subject: Third~%~%c~%"))
                       (cons "Inbox/4.eml" (format nil "Subject: Third~%~%d~%")))))
      ;; one Message-ID forged or copied, headers alike: each message its own identity
      (multiple-value-bind (list job) (mailbox-test-list dir twins :sum sum)
        (let ((identities (mapcar #'outlook::mailbox-item-identity (cdr (first (outlook::mailbox-job-folders job))))))
          (is = 4 (length (remove-duplicates identities :test #'string=)))
          (true (search (format nil "~c2" #\Tab) (fourth identities))))
        (tuatara.buffer:kill-buffer list))
      ;; two sessions with the same mailbox open, each labelling: neither's labels lost
      (multiple-value-bind (one one-job) (mailbox-test-list (merge-pathnames "one/" dir) *mailbox-label-test-messages* :sum sum)
        (multiple-value-bind (two two-job) (mailbox-test-list (merge-pathnames "two/" dir) *mailbox-label-test-messages* :sum sum)
          (flet ((item (job subject) (find subject (cdr (first (outlook::mailbox-job-folders job)))
                                           :key #'outlook::mailbox-item-subject :test #'string=)))
            (tuatara.tests::run-keys
             (list (lambda ()
                     (outlook::mailbox-relabel one-job (list (item one-job "First")) (lambda (had) (append had '("from one"))))
                     (outlook::mailbox-relabel two-job (list (item two-job "Second")) (lambda (had) (append had '("from two"))))
                     (outlook::mailbox-relabel two-job (list (item two-job "Second")) (lambda (had) (remove "from two" had :test #'string=)))
                     (outlook::mailbox-relabel two-job (list (item two-job "Third")) (lambda (had) (append had '("Kea")))))
                   #'wait-for-mailbox-labels :tick #'wait-for-mailbox-labels :tick)
             :width 140 :height 30))
          (tuatara.buffer:kill-buffer two))
        (tuatara.buffer:kill-buffer one))
      (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames "three/" dir) *mailbox-label-test-messages* :sum sum)
        (is equal '(("from one") nil ("Kea"))
            (mapcar (lambda (item) (outlook::mailbox-item-labels job item)) (cdr (first (outlook::mailbox-job-folders job)))))
        ;; told apart by case: kea is not Kea; a label put on while the filter is on shows at once
        (mailbox-test-label job "First" "kea")
        (outlook::mailbox-render list job)
        (is equal '("First")
            (nth-value 1 (mailbox-search-session
                          list (list (lambda () (tuatara:add-search-filter list (outlook::mailbox-labels-pattern '("kea"))
                                                                           :part "your label" :ignore-case nil))
                                     :searched :look))))
        (let ((seen nil))
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer list))
                 (lambda ()
                   (outlook::mailbox-relabel job (list (second (cdr (first (outlook::mailbox-job-folders job)))))
                                             (lambda (had) (append had '("kea")))))
                 #'wait-for-mailbox-search :tick (lambda () (setf seen (mailbox-subjects list))))
           :width 140 :height 30)
          (is equal '("First" "Second") seen))
        (tuatara.buffer:kill-buffer list))
      ;; a journal grown long is written again shorter, the same labels left
      (let ((path (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
        (dotimes (i 300)
          (tuatara.io:append-to-file path (format nil "+~cx~cA~%-~cx~cA~%" #\Tab #\Tab #\Tab #\Tab)))
        (let ((before (length (outlook::mailbox-journal-lines path)))
              (labels (outlook::mailbox-read-labels (outlook::mailbox-labels-name sum))))
          (true (> before 600))
          (true (< (length (outlook::mailbox-journal-lines path)) 20))
          (is = (hash-table-count labels) (hash-table-count (outlook::mailbox-replay-labels (outlook::mailbox-journal-lines path)))))))))

(define-test (outlook mailbox-labels-journal-mended-and-apart)
  (with-test-directory (dir)
    ;; a last line cut short by a crash: the next change starts on a line of its own
    (let ((path (namestring (merge-pathnames "journal" dir))))
      (tuatara.io:append-to-file path (format nil "+~ca~cX" #\Tab #\Tab))
      (outlook::mailbox-append-lines path (list (outlook::mailbox-label-line "+" "b" "Y")))
      (let ((table (outlook::mailbox-replay-labels (outlook::mailbox-journal-lines path))))
        (is equal '("a") (gethash "X" table))
        (is equal '("b") (gethash "Y" table))))
    ;; read with deleted items or without: two journals, as readpst's order differs
    (is string= "kea" (let ((tuatara-user:*outlook-include-deleted* nil)) (outlook::mailbox-labels-name "kea")))
    (is string= "kea.with-deleted" (let ((tuatara-user:*outlook-include-deleted* t)) (outlook::mailbox-labels-name "kea")))))

;;; Filtered by one field of the messages

(defun mailbox-field-test-messages ()
  "The messages filtered by a field in the tests: headers many times and of
every kind, attachments, a subject with a regex's characters, and an
Outlook item beside the first."
  (list (cons "Inbox/1.eml"
              (format nil "From: Kea <kea@example.org>~%To: Ruru <ruru@example.org>~%Subject: First [draft] plan~%~
                           Received: from a.example.org by b.example.net~%~
                           Received: from relay.example.org (203.0.113.9) by a.example.org~%~
                           X-Mailer: Kererū Mail 2~%Message-ID: <one@example.org>~%~
                           Attachment: budget, as a header~%Outlook importance: high~%~%one~%"))
        (cons "Inbox/1.msg" (msg-test-octets))
        (cons "Inbox/2.eml"
              (format nil "From: Tui <tui@example.org>~%To: Kea <kea@example.org>~%Cc: Weka <weka@example.org>~%~
                           Subject: Second d plan~%Received: from c.example.org by d.example.net~%~
                           In-Reply-To: <one@example.org>~%MIME-Version: 1.0~%~
                           Content-Type: multipart/mixed; boundary=\"b\"~%~%--b~%Content-Type: text/plain~%~%two~%~
                           --b~%Content-Type: application/pdf; name=\"budget 2026.pdf\"~%~
                           Content-Transfer-Encoding: base64~%~%JVBERi0xLjQK~%--b--~%"))
        (cons "Inbox/3.eml" (format nil "From: Weka <weka@example.org>~%Subject: Third~%~%three~%"))))

(define-test (outlook mailbox-filtered-by-fields)
  (with-test-directory (dir)
    (multiple-value-bind (list job) (mailbox-test-list dir (mailbox-field-test-messages))
      (flet ((shown (keys)
               (nth-value 1 (mailbox-search-session list (append (list "C-x / r") keys (list :searched :look))))))
        ;; the fields found, the usual ones first, every header and the Outlook item's
        (let ((names (outlook::mailbox-field-names (outlook::mailbox-job-field-names job))))
          (is equal '("from" "to" "cc" "bcc" "subject" "date" "attachment name" "your label")
              (mapcar #'car (subseq names 0 8)))
          (dolist (name '("received" "x-mailer" "message-id" "in-reply-to" "attachment" "outlook message-class"
                          "outlook sender-address-type" "outlook client-submit-time" "outlook message-flags"))
            (true (assoc name names :test #'string=) "~a offered" name))
          ;; a header's name never one Tuatara makes up: Attachment: is a header, and one with a space no field
          (is string= "a header" (cdr (assoc "attachment" names :test #'string=)))
          (is string= "the name of an attachment" (cdr (assoc "attachment name" names :test #'string=)))
          (false (assoc "outlook importance" names :test #'string=)))
        ;; one field alone: Received, any of many; a field a message has not is no match
        (is equal '("First") (shown '("C-x / f r e c e i v e d RET 2 0 3 \\ . 0 \\ . 1 1 3 RET")))
        (is equal '("First" "Second") (shown '("C-x / f r e c e i v e d RET e x a m p l e RET")))
        (is equal '("Second") (shown '("C-x / f c c RET w e k a RET")))
        (is equal '("Second") (shown '("C-x / f t o RET k e a RET")))
        (is equal '() (shown '("C-x / f t o RET w e k a RET")))
        (is equal '("Second") (shown '("C-x / f i n - r e p l y - t o RET o n e RET")))
        (is equal '("Second") (shown '("C-x / f a t t a c h m e n t SPC n a m e RET b u d g e t RET")))
        (is equal '("First") (shown '("C-x / f a t t a c h m e n t RET b u d g e t RET")))
        (is equal '("First") (shown '("C-x / f x - m a i l e r RET k e r e r RET")))
        ;; what the Outlook item says of itself
        (is equal '("First") (shown '("C-x / f o u t l o o k SPC m e s s a g e - c l a s s RET I P M RET")))
        (is equal '("First") (shown '("C-x / f o u t l o o k SPC s e n d e r - a d d r e s s - t y p e RET ^ E X $ RET")))
        ;; a regular expression by default, literal text with M-r
        (is equal '("First" "Second" "Third") (shown '("C-x / f s u b j e c t RET [ d r a f t ] RET")))
        (is equal '("First") (shown '("C-x / f s u b j e c t RET M-r [ d r a f t ] RET")))
        (is equal '("Third") (shown '("C-x / h s u b j e c t RET p l a n RET")))
        ;; with the other filters
        (is equal '("Second") (shown '("C-s p l a n RET" :searched "C-x / f c c RET . RET")))
        ;; a named pattern saved as literal text, used by its name
        (unwind-protect
             (progn (tuatara.tests::run-keys
                     (list "M-x s a v e - n a m e d - p a t t e r n RET M-r [ d r a f t ] RET b r a c k e t e d RET RET"))
                    (is equal '("First") (shown '("C-x / f s u b j e c t RET [ [ : b r a c k e t e d : ] ] RET"))))
          (tuatara.defaults::state-list-drop tuatara.defaults::*saved-patterns* (list "bracketed"))
          (remhash "bracketed" tuatara.syntax::**own-named-patterns**)))
      ;; the message opened lights the header's line that matched, from its name
      (setf tuatara.defaults::*search-lit* nil)
      (mailbox-search-session list (list "C-x / r C-x / f r e c e i v e d RET r e l a y RET" :searched))
      (tuatara:light-search-filters list list)
      (has (tuatara.defaults::search-state-string (car tuatara.defaults::*search-lit*)) "(?m:^(?i:received):[^\\n]*?(?:relay))")
      (setf tuatara.defaults::*search-lit* nil tuatara.defaults::*search-lit-shown* nil)
      (tuatara.buffer:kill-buffer list))))

;;; Copied out: all shown, and into one mbox file

(define-test (outlook mailbox-copied-out-shown-and-as-mbox)
  (with-test-directory (dir)
    (multiple-value-bind (list job)
        (mailbox-test-list dir (list (cons "Inbox/1.eml" (format nil "From: Kea <kea@example.org>~%Subject: First~%~
                                                                       Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%~
                                                                       From the start~%>From quoted~%Fromage~%no end"))
                                     (cons "Inbox/2.eml" (format nil "Return-Path: <ruru@example.org>~%Subject: Second~%~%two~%"))
                                     (cons "Inbox/3.eml" (format nil "Subject: Third~%~%three~%"))))
      (declare (ignore job))
      (let ((copies (namestring (merge-pathnames "copies/" dir)))
            (mbox (namestring (merge-pathnames "all.mbox" dir))))
        ;; narrowed to two, one marked: a, All shown, copies the two the filter leaves
        (tuatara.tests::run-keys
         (list (lambda () (tuatara:show-buffer list))
               "C-s ^ s u b j e c t : SPC ( F i r s t | T h i r d ) RET" #'wait-for-mailbox-search :tick
               (mailbox-test-go-to list "Third") "m" "TAB"
               (format nil "C a e C-a C-k ~a C-j" (tuatara.tests::rimu-typed copies))
               #'wait-for-mailbox-export :tick)
         :width 140 :height 30)
        (is equal (list (format nil "~a First.eml" (tuatara:format-time "%F %H%M" (encode-universal-time 0 0 9 6 10 2026 0)))
                        "Third.eml")
            (sort (mapcar #'first (tuatara.io:directory-entries copies)) #'string<))
        ;; b: the one marked into one mbox file; again, a name taken gets (2)
        (dolist (keys (list (format nil "C b C-a C-k ~a C-j" (tuatara.tests::rimu-typed mbox))
                            (format nil "C a b C-a C-k ~a C-j" (tuatara.tests::rimu-typed mbox))))
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer list)) "C-x / r" (mailbox-test-go-to list "First") "U m" keys
                 #'wait-for-mailbox-export :tick)
           :width 140 :height 30))
        (let ((one (uiop:read-file-string mbox))
              (all (uiop:read-file-string (merge-pathnames "all (2).mbox" dir))))
          (is string= (format nil "From kea@example.org Tue Oct  6 09:00:00 2026~%From: Kea <kea@example.org>~%~
                                   Subject: First~%Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%~
                                   >From the start~%>>From quoted~%Fromage~%no end~%~%")
              one)
          ;; all of them, each after its From line, the address it came back to first
          (is = 3 (/ (length (cl-ppcre:all-matches "(?m)^From " all)) 2))
          (has all (format nil "~%From ruru@example.org "))
          (has all (format nil "~%From MAILER-DAEMON "))))
      (tuatara.buffer:kill-buffer list))))

;;; Attachments earlier in a conversation

(defun mailbox-thread-test-messages ()
  "Two conversations: one linked by Message-ID and In-Reply-To, its first
message carrying a PDF; one by Outlook's conversation index, its first a
picture; and a message of neither."
  (flet ((with-attachment (headers name type)
           (format nil "~aMIME-Version: 1.0~%Content-Type: multipart/mixed; boundary=\"b\"~%~%--b~%~
                        Content-Type: text/plain~%~%see attached~%--b~%Content-Type: ~a; name=\"~a\"~%~
                        Content-Transfer-Encoding: base64~%~%JVBERi0xLjQK~%--b--~%"
                   headers type name)))
    (list (cons "Inbox/1.eml" (with-attachment (format nil "Subject: First~%Message-ID: <first@example.org>~%~
                                                           Date: Tue, 6 Oct 2026 09:00:00 +0000~%")
                                               "budget.pdf" "application/pdf"))
          (cons "Inbox/2.eml" (format nil "Subject: Second~%Message-ID: <second@example.org>~%~
                                           In-Reply-To: <first@example.org>~%~%Re: the budget~%"))
          (cons "Inbox/3.eml" (format nil "Subject: Third~%References: <first@example.org> <second@example.org>~%~
                                           Message-ID: <third@example.org>~%~%Re: Re: the budget~%"))
          (cons "Inbox/4.eml" (with-attachment (format nil "Subject: Fourth~%Thread-Index: AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQ==~%")
                                               "map.pdf" "application/pdf"))
          (cons "Inbox/5.eml" (format nil "Subject: Fifth~%Thread-Index: AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQICAgIC~%~%fwd~%"))
          (cons "Inbox/6.eml" (format nil "Subject: Sixth~%~%alone~%")))))

(define-test (outlook mailbox-earlier-attachments)
  (with-test-directory (dir)
    (multiple-value-bind (list job) (mailbox-test-list dir (mailbox-thread-test-messages))
      (let* ((items (cdr (first (outlook::mailbox-job-folders job))))
             (tables (outlook::mailbox-job-threads job)))
        (flet ((earlier (n) (mapcar (lambda (found) (outlook::mailbox-item-subject (car found)))
                                    (outlook::mailbox-earlier-items tables (nth n items))))
               (how (n) (mapcar #'cdr (outlook::mailbox-earlier-items tables (nth n items)))))
          ;; by In-Reply-To and References, followed back; by the conversation index; none for the first or one alone
          (is equal '("First") (earlier 1))
          (is equal '("First") (earlier 2))
          (is equal '("Fourth") (earlier 4))
          (is equal '() (earlier 0))
          (is equal '() (earlier 5))
          ;; each says how it was found
          (is equal '("by In-Reply-To") (how 1))
          (is equal '("by References") (how 2))
          (is equal '("by conversation index: header") (how 4))))
      ;; one Message-ID given to two messages: both found, and said
      (multiple-value-bind (list job)
          (mailbox-test-list (merge-pathnames "twice/" dir)
                             (append (mailbox-thread-test-messages)
                                     (list (cons "Inbox/7.eml"
                                                 (format nil "Subject: Seventh~%Message-ID: <first@example.org>~%~
                                                              Date: Wed, 7 Oct 2026 09:00:00 +0000~%MIME-Version: 1.0~%~
                                                              Content-Type: multipart/mixed; boundary=\"b\"~%~%--b~%~
                                                              Content-Type: text/plain~%~%x~%--b~%~
                                                              Content-Type: application/pdf; name=\"other.pdf\"~%~
                                                              Content-Transfer-Encoding: base64~%~%JVBERi0xLjQK~%--b--~%")))))
        (let ((found (outlook::mailbox-earlier-items (outlook::mailbox-job-threads job)
                                                     (second (cdr (first (outlook::mailbox-job-folders job)))))))
          (is equal '("First" "Seventh") (mapcar (lambda (f) (outlook::mailbox-item-subject (car f))) found))
          (true (every (lambda (f) (search "<first@example.org> is the Message-ID of 2 messages" (cdr f))) found)))
        (tuatara.buffer:kill-buffer list))
      ;; the reply opened: the first's attachment at its end, opened as the message's own are
      (let ((shown nil) (opened nil))
        (tuatara.tests::run-keys
         (list (lambda () (tuatara:show-buffer list)) (mailbox-test-go-to list "Third") "RET"
               #'wait-for-mail-reading :tick
               (lambda () (setf shown (tuatara.buffer:buffer-string (tuatara:current-buffer))))
               (lambda ()
                 (let ((buffer (tuatara:current-buffer)))
                   (setf (tuatara.buffer:buffer-point buffer)
                         (first (first (tuatara.buffer:buffer-local buffer 'outlook::mail-attachment-places))))))
               "RET" (lambda () (setf opened (tuatara.buffer:buffer-name (tuatara:current-buffer)))))
         :width 140 :height 40)
        (has shown "Attachments earlier in this conversation: 1")
        (has shown "(by References)")
        (has shown "First")
        (has shown "budget.pdf")
        (true (search "budget" opened) "~a" opened))
      (tuatara.buffer:kill-buffer list))))

;;; Marked, folded, opened and copied out

(define-test (outlook mailbox-marked-while-narrowed)
  ;; a mark given or a folder folded in a narrowed list keeps it narrowed, nothing searched again
  (with-test-directory (dir)
    (let ((list (mailbox-search-test-list dir)) (after-mark nil) (after-fold nil) (jobs nil))
      (tuatara.tests::run-keys
       (list (lambda () (tuatara:show-buffer list))
             "C-s g a n n e t | a l b a t r o s s RET" #'wait-for-mailbox-search :tick
             (lambda () (point-to-line-of list "Second")) "m"
             (lambda () (setf after-mark (tuatara.buffer:buffer-string list)
                              jobs (count-if #'tuatara.defaults::search-filter-job-p tuatara.runtime:*jobs*)))
             (lambda () (point-to-line-of list "Outlook Data File/Inbox")) "RET"
             (lambda () (setf after-fold (tuatara.buffer:buffer-string list))))
       :width 140 :height 30)
      (has after-mark "Filtered by gannet|albatross: 2 of 5 items")
      (is = 0 jobs)
      (is equal '("Second" "Third") (let ((seen '()))
                                      (dolist (s '("First" "Second" "Third" "Fourth" "Fifth") (nreverse seen))
                                        (when (search s after-mark) (push s seen)))))
      (true (search "> " after-mark))
      (has after-fold "Filtered by gannet|albatross")
      (has after-fold "▸ Outlook Data File/Inbox")
      (false (search "Second" after-fold))
      (tuatara.buffer:kill-buffer list))))

(defun wait-for-mail-reading ()
  "Wait, a few seconds at most, till no message is being read."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (outlook::mail-read-job-p job) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(defun wait-for-mailbox-export ()
  "Wait, a few seconds at most, till no messages are being copied out of a mailbox."
  (let ((until (+ (get-internal-real-time) (* 10 internal-time-units-per-second))))
    (loop while (and (some (lambda (job) (and (outlook::mailbox-export-job-p job) (tuatara.runtime:job-running-p job)))
                           tuatara.runtime:*jobs*)
                     (< (get-internal-real-time) until))
          do (sleep 0.01))))

(define-test (outlook mailbox-marked-and-copied-out)
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "out/" dir))
           (pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                   (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
           (list (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
           (job (outlook::make-mailbox-job list pst (namestring out)))
           (sent (encode-universal-time 0 0 9 6 10 2026 0)))
      (flet ((put (name text)
                  (put-test-file (merge-pathnames (concatenate 'string "Outlook Data File/" name) out) text))
             (copied (directory) (sort (mapcar (lambda (p) (tuatara.io:native-file-name (namestring p)))
                                            (directory (merge-pathnames "*.*" directory)))
                                    #'string<))
             (text-of (directory name) (uiop:read-file-string (merge-pathnames name directory))))
        ;; readpst -m writes each message as .eml and .msg, and leaves temp_file_attachment behind
        (put "Inbox/1.eml"
             (format nil
                     "From: Kea <kea@example.org>~%Subject: Re: a/b?~%Date: Tue, 6 Oct 2026 09:00:00 +0000~%~%one~%"))
        (put "Inbox/1.msg" "the first as an Outlook item")
        (put "Inbox/2.eml" (format nil "Subject: Second~%~%two~%"))
        (put "Inbox/temp_file_attachment" "left over")
        (put "Sent Items/1.eml" (format nil "Subject: Third~%~%three~%"))
        (put "Sent Items/1.msg" "the third as an Outlook item")
        (outlook::mailbox-list job)
        (let* ((inbox (cdr (assoc "Outlook Data File/Inbox" (outlook::mailbox-job-folders job) :test #'string=)))
               (first (first inbox)) (second (second inbox)))
          (is equal '("Re: a/b?" "Second") (mapcar #'outlook::mailbox-item-subject inbox))
          (true (search "Inbox/1.msg" (outlook::mailbox-item-msg first)))
          (false (outlook::mailbox-item-msg second))
          (is = 3 (reduce #'+ (outlook::mailbox-job-folders job) :key (lambda (f) (length (cdr f)))))
          (tuatara.modes:set-major-mode list 'outlook::mailbox-mode)
          (setf (tuatara.buffer:buffer-local list 'outlook::mailbox-path) pst)
          (outlook::mailbox-render list job)
          (let ((shown '()) (opened nil) (moved nil) (back nil) (marked nil) (stayed nil)
                (copies (namestring (merge-pathnames "copies/" dir))))
            (tuatara.tests::run-keys
             (list (lambda () (tuatara:show-buffer list))
                   ;; h asks before it goes back to the directory: no, and the list stays
                   "h n" (lambda () (setf stayed (tuatara:current-buffer)))
                   ;; m marks the message on the line and moves down; RET and l on a folder's line fold it
                   "m" (lambda () (setf marked (tuatara.defaults::list-marked list :marked)))
                   "k k RET" (lambda () (push (tuatara.buffer:buffer-string list) shown))
                   "l" (lambda () (push (tuatara.buffer:buffer-string list) shown))
                   ;; l opens a message, q comes back to the list, h only moving the cursor
                   "j l" (lambda () (setf opened (tuatara.buffer:buffer-name (tuatara:current-buffer))))
                   "h" (lambda () (setf moved (tuatara:current-buffer)))
                   "q" (lambda () (setf back (tuatara:current-buffer)))
                   ;; C opens the menu of copying out: m copies the one marked as an Outlook item,
                   ;; into a directory asked for, made for it
                   (format nil "C m C-a C-k ~a C-j" (tuatara.tests::rimu-typed copies))
                   #'wait-for-mailbox-export :tick)
             :width 140 :height 30)
            (is equal (list first) marked)
            (true (search "> 2026-10-06" (tuatara.buffer:buffer-string list)))
            ;; the line over the list says the keys, those of a message opened from it too
            ;; moves by word and selections but V's do nothing in the list
            (dolist (keys '("w" "b" "W" "B" "v" "%"))
              (is eq :undefined (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'outlook::mailbox-mode)
                                                            (tuatara.frontend:parse-key-sequence keys))))
            (false (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'outlook::mailbox-mode)
                                               (tuatara.frontend:parse-key-sequence "V")))
            (is eq list stayed)
            (has (tuatara.buffer:buffer-string list)
                 "RET or l opens the item on the line, a folder's line folds; P in a")
            ;; filled to the help column, so that the line reads whole in a narrow window
            (true (every (lambda (line) (<= (length line) tuatara:*help-column*))
                         (subseq (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline)) 1 4))
                  "the words over the list are filled")
            (has (first (rest shown)) "▸ Outlook Data File/Inbox (2)")
            (false (search "Second" (first (rest shown))))
            (has (first shown) "Second")
            (is string= "Re: a/b?" opened)
            (is string= "Re: a/b?" (tuatara.buffer:buffer-name moved))
            (is eq list back)
            (let ((name (format nil "~a Re a-b.msg" (tuatara:format-time "%F %H%M" sent))))
              (is equal (list name) (copied copies))
              (is string= "the first as an Outlook item" (text-of copies name))))
          ;; as saved mail, a name taken getting (2); one with no Outlook item written is said
          (let ((copies (namestring (merge-pathnames "more/" dir))))
            (ensure-directories-exist copies)
            (dolist (type '("eml" "eml" "msg"))
              (outlook::mailbox-export-work (outlook::make-mailbox-export-job list (list first second) copies type)))
            (let ((stamp (tuatara:format-time "%F %H%M" sent)))
              (is equal (sort (list (format nil "~a Re a-b.eml" stamp) (format nil "~a Re a-b (2).eml" stamp)
                                    (format nil "~a Re a-b.msg" stamp) "Second.eml" "Second (2).eml")
                              #'string<)
                  (copied copies)))
            (let ((job (outlook::make-mailbox-export-job list (list second) copies "msg")))
              (outlook::mailbox-export-work job)
              (is = 0 (outlook::mailbox-export-job-copied job))
              (has (cdr (first (outlook::mailbox-export-job-not-copied job))) "no Outlook item"))))
        ;; P: the message on the line beside the list, read as any message is into a buffer
        ;; of the preview's own; the next one's as the cursor moves on; P again, none
        (let ((shown '()) (after t))
          (flet ((note-preview ()
                   (let* ((preview tuatara.defaults::*entry-preview*)
                          (buffer (and preview (tuatara.defaults::side-preview-buffer preview))))
                     (push (and buffer (list (tuatara.buffer:buffer-name buffer) (tuatara.buffer:buffer-string buffer)
                                             (type-of (tuatara.modes:buffer-major-mode buffer))))
                           shown))))
            (tuatara.tests::run-keys
             (list (lambda () (setf (tuatara.buffer:buffer-local list :list-preview) nil) (tuatara:show-buffer list))
                   "g g n P" #'wait-for-mail-reading :tick :tick #'note-preview
                   "n" #'wait-for-mail-reading :tick :tick #'note-preview
                   "P" (lambda () (setf after tuatara.defaults::*entry-preview*)))
             :width 140 :height 30))
          (destructuring-bind (second first) shown
            (is string= "Re: a/b? (preview)" (first first))
            (has (second first) "one")
            (is eq 'outlook::mail-view-mode (third first))
            (is string= "Second (preview)" (first second))
            (has (second second) "two"))
          (false after)
          (false (find-if (lambda (b) (search "(preview)" (tuatara.buffer:buffer-name b))) tuatara.buffer:*buffers*)))
        ;; h, answered yes: the list closed, its directory listed by rimu, point on the mailbox
        (let ((directory nil))
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer list))
                 "h y" (lambda () (setf directory (tuatara:current-buffer))))
           :width 140 :height 30)
          (false (tuatara.buffer:live-buffer-p list))
          (true (tuatara.defaults::rimu-list-p directory))
          (is string= "archive.pst" (tuatara.io:native-file-name (tuatara.defaults::shown-file directory)))
          (tuatara.buffer:kill-buffer directory))
        (when (tuatara.buffer:live-buffer-p list) (tuatara.buffer:kill-buffer list))))))

(define-test (outlook mailbox-without-readpst)
  (with-test-directory (dir)
    (let ((pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                  (concatenate '(vector (unsigned-byte 8))
                                               (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)
                                               (make-array 600 :initial-element 0)))))
      (let ((tuatara-user:*outlook-readpst-program* "no-such-readpst"))
        ;; opened as any file, saying how to get readpst
        (multiple-value-bind (f editor) (outlook-session pst '())
          (true (tuatara.frontend.fake:fake-frame-showing f "libpst"))
          (is string= "archive.pst" (tuatara.buffer:buffer-name (session-buffer editor)))
          (tuatara.buffer:kill-buffer (session-buffer editor)))
        ;; started with it: opened as C-x C-f opens it, in the hex view, saying the same
        (let ((f (tuatara.frontend.fake:make-fake-frontend :width 400 :height 20)))
          (tuatara.command:run-editor f :files (list (namestring pst)) :setup #'tuatara.defaults:install-defaults)
          (true (tuatara.frontend.fake:fake-frame-showing f "Hex 0x00000000"))
          (true (tuatara.frontend.fake:fake-frame-showing f "libpst"))
          (tuatara.buffer:kill-buffer (tuatara.buffer:find-buffer "archive.pst")))
          (has (outlook::outlook-health-words) "libpst")
          (has (outlook::mailbox-preview-words "archive.pst" 2048) "not installed"))
        (if (not (outlook::mailbox-readpst))
          (skip "readpst is not installed here" (true nil))
          (progn
              (has (outlook::outlook-health-words) "shut in")
            ;; readpst reads a copy: the mailbox's access time kept, the copy gone after
            (sb-posix:utimes (namestring pst) 946681200 946681200)
            (multiple-value-bind (f editor) (outlook-session pst '())
              (declare (ignore f))
              (tuatara.buffer:kill-buffer (session-buffer editor)))
            (is = 946681200 (sb-posix:stat-atime (sb-posix:stat (namestring pst))))
            (false (directory (merge-pathnames "*.pst" (tuatara.defaults::private-cache-directory "mailboxes/")))))))))

;;; Previews, loading

(define-test (outlook previews-and-loading)
  (with-test-directory (dir)
    (let ((path (write-test-octets (merge-pathnames "survey.msg" dir) (msg-test-octets))))
      (tuatara.io:call-with-file path
                                 (lambda (fd name size)
                                     (let ((text (outlook::outlook-preview-text fd
                                                                                name
                                                                                size
                                                                                :ole (tuatara.io:read-fd-start fd
                                                                                                               name
                                                                                                               size
                                                                                                               4096))))
                                     (has text "Kea survey results")
                                     (has text "kea.png")
                                     (has text "Twelve kea"))))))
  ;; a path is no package's name
  (fail (tuatara-user:load-package "../outlook"))
  ;; the package's views and preview in place
  (true (member 'outlook::mail-visit tuatara.defaults::*file-views*))
  (true (member 'outlook::outlook-preview-text tuatara.defaults::*preview-text-functions*))
  (true (member 'outlook::outlook-health-words tuatara.defaults::*health-decoder-functions*)))

(define-test (outlook shared-and-deep)
  ;; an entry two storages name belongs to the first alone, and streams coming to more than twice the file
  ;; are refused, so that one stream is never copied for each storage that names it
    (let* ((entries (vector (outlook::make-cfb-entry :name "Root" :type :root :child 1 :index 0)
                            (outlook::make-cfb-entry :name "a" :type :storage :right 2 :child 3 :index 1)
                            (outlook::make-cfb-entry :name "b" :type :storage :child 3 :index 2)
                            (outlook::make-cfb-entry :name "shared" :type :stream :start 0 :size 60 :index 3)))
           (cfb (outlook::%make-cfb :octets (make-array 1024 :element-type '(unsigned-byte 8))
                                    :sector-size 512
                                    :sectors 1
                                           :entries entries :cutoff 4096
                                           :mini-stream (make-array 4096 :element-type '(unsigned-byte 8))
                                           :mini-fat (make-array 64
                                                                 :element-type '(unsigned-byte 32)
                                                                 :initial-element #xFFFFFFFE))))
      (is equal '("shared") (mapcar #'outlook::cfb-entry-name (outlook::cfb-children cfb (aref entries 1))))
      (false (outlook::cfb-children cfb (aref entries 2)))
      (setf (outlook::cfb-entry-size (aref entries 3)) 60)
      (is = 60 (length (outlook::cfb-stream-octets cfb (aref entries 3))))
      (setf (outlook::cfb-given cfb) 2000)
      (fail (outlook::cfb-stream-octets cfb (aref entries 3))))
  ;; RTF nested a hundred thousand deep: read at once, not in time growing with the square of it
  (let ((start (get-internal-real-time)))
    (outlook::rtf-text (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 123))
    (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second)))))

;;; Parts counted, messages sent encoded, attachments, readpst's files

(defun outlook-eml (text)
  "The mail TEXT (lines as FORMAT writes them) read as a saved mail."
  (outlook::eml-read (sb-ext:string-to-octets text :external-format :utf-8)))

(defun outlook-multipart (count)
  "A saved mail's text of COUNT plain parts, each named a.txt."
  (with-output-to-string (out)
    (format out "Subject: parts~%Content-Type: multipart/mixed; boundary=b~%~%")
    (dotimes (k count) (format out "--b~%Content-Type: text/plain; name=a.txt~%~%part ~d~%" k))
    (format out "--b--~%")))

(define-test (outlook parts-counted-in-the-whole-message)
  ;; as many parts as may be read, all read: nothing said to be left out; one more, and it is said
  (let ((outlook::*mime-part-limit* 5))
    (let ((mail (outlook-eml (outlook-multipart 5))))
      (is = 5 (length (outlook::mail-attachments mail)))
      (false (find-if (lambda (n) (search "Only its first" n)) (outlook::mail-notes mail))))
    (let ((mail (outlook-eml (outlook-multipart 7))))
      (is = 5 (length (outlook::mail-attachments mail)))
      (true (find-if (lambda (n) (search "Only its first 5 parts" n)) (outlook::mail-notes mail)))))
  ;; the limit is the message's, its multiparts together
  (let ((outlook::*mime-part-limit* 4)
        (inner (format nil
                       "Content-Type: multipart/mixed; boundary=i~%~%--i~%Content-Type: text/plain; name=a.txt~%~%one~%--i~%Content-Type: text/plain; name=b.txt~%~%two~%--i--~%")))
    (let ((mail (outlook-eml (format nil
                                     "Content-Type: multipart/mixed; boundary=b~%~%--b~%~a--b~%~a--b--~%"
                                     inner
                                     inner))))
      (is = 3 (length (outlook::mail-attachments mail)))
      (true (find-if (lambda (n) (search "Only its first 4 parts" n)) (outlook::mail-notes mail))))))

(define-test (outlook attached-message-sent-encoded)
  ;; a message attached in base64 is read as a message
  (let* ((inner (format nil "Subject: Kea survey~%From: tui@example.org~%~%The count is in.~%"))
         (mail (outlook-eml (format nil
                                    "Subject: forwarded~%Content-Type: multipart/mixed; boundary=b~%~%--b~%Content-Type: text/plain~%~%See below.~%~
                                         --b~%Content-Type: message/rfc822~%Content-Transfer-Encoding: base64~%~%~a~%--b--~%"
                                    (tuatara.io:base64-string (tuatara.text:string-octets inner)))))
         (attachment (first (outlook::mail-attachments mail))))
    (true attachment)
    (is string= "Kea survey" (outlook::mail-attachment-name attachment))
    (true (outlook::mail-attachment-message attachment))
    (is string=
        "The count is in."
        (string-trim '(#\Newline) (outlook::mail-body (outlook::mail-attachment-message attachment))))))

(define-test (outlook parameters-split)
  ;; a backslash in quotes takes the character after it, a ; there among them
  (is equal
      '("text/plain" " name=\"a\\\";b\"" " charset=utf-8")
      (outlook::mime-split-parameters "text/plain; name=\"a\\\";b\"; charset=utf-8")))

(define-test (outlook attribute-scanners-made-once)
  ;; an attribute looked up makes no scanner of its own
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (k 10000) (outlook::html-attribute "<a class=x href=\"https://example.org/\">" "href"))
    (true (< (- (sb-ext:get-bytes-consed) before) (* 10000 3 1024))))
  (is string= "https://example.org/" (outlook::html-attribute "<a class=x href=\"https://example.org/\">" "href")))

(define-test (outlook attachment-unlike-its-name-told)
  ;; an attachment whose contents are not what its name says is told as a second value, not by its words
  (true (nth-value 1
                   (outlook::mail-attachment-words (outlook::make-mail-attachment :name "invoice.pdf"
                                                                                  :octets *png-test-octets*))))
  (false (nth-value 1
                    (outlook::mail-attachment-words (outlook::make-mail-attachment :name "kea.png"
                                                                                   :octets *png-test-octets*)))))

(define-test (outlook readpst-files-counted)
  ;; every file readpst wrote counted, folders within folders too; the list told when it holds only some
  (with-test-directory (dir)
    (let ((root (namestring dir)))
      (dolist (name '("Inbox/1.eml" "Inbox/2.eml" "Inbox/Old/3.eml"))
        (let ((path (tuatara.io:path-join root name)))
          (tuatara.io:ensure-directories path)
          (tuatara.io:replace-file path "kea")))
      (multiple-value-bind (files octets) (outlook::mailbox-written root)
        (is = 3 files)
        (is = 9 octets))
      (multiple-value-bind (folders more) (outlook::mailbox-folders root :limit 2)
        (is = 2 (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
        (true more))
      (false (nth-value 1 (outlook::mailbox-folders root))))))

(define-test (outlook mailbox-files-taken-in-set-by-setting)
  ;; the most files a mailbox's list takes in is a setting; past it the list says so
  (with-test-directory (dir)
    (let* ((out (merge-pathnames "out/" dir))
           (pst (write-test-octets (merge-pathnames "archive.pst" dir)
                                   (sb-ext:string-to-octets "!BDN    SM" :external-format :latin-1)))
           (buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "archive.pst (mailbox)")))
           (job (outlook::make-mailbox-job buffer pst (namestring out))))
      (dolist (n '(1 2 3))
        (put-test-file (merge-pathnames (format nil "Outlook Data File/Inbox/~d.eml" n) out)
                       (format nil "Subject: Item ~d~%~%text~%" n)))
      (let ((tuatara-user:*outlook-mailbox-most-files* 2))
        (outlook::mailbox-list job))
      (is = 2 (outlook::mailbox-job-total job))
      (true (find-if (lambda (note) (search "*outlook-mailbox-most-files*" note)) (outlook::mailbox-job-notes job))))))

(define-test (outlook mailbox-counted-less-often-when-large)
  ;; what readpst has written is counted each second at first, and no more often than every thirty
  ;; seconds once there are many files, each count walking them all
  (is = 1 (outlook::mailbox-count-seconds 0))
  (is = 1 (outlook::mailbox-count-seconds 3000))
  (is = 5 (outlook::mailbox-count-seconds 10000))
  (is = 30 (outlook::mailbox-count-seconds 1000000)))

(define-test (outlook outlook-keys-under-c-c-o)
  ;; its commands under a prefix of its own, C-c o, as every package's are, beside its lists' own letters
  (loop for (mode keys command) in '((outlook::mail-view-mode "C-c o o" outlook::mail-open-attachment-outside)
                                     (outlook::mail-view-mode "C-c o t" outlook::mail-view-file-text)
                                     (outlook::mail-view-mode "C-c o p" outlook::mail-view-pages)
                                     (outlook::mailbox-mode "C-c o h" outlook::mailbox-back-to-directory)
                                     (outlook::mailbox-mode "C-c o c" outlook::mailbox-export))
        do (is eq
               command
               (tuatara.command:lookup-key (tuatara:mode-keymap mode) (tuatara.frontend:parse-key-sequence keys))
               "~a"
               keys)))

(define-test (outlook nested-boundary-that-begins-as-the-outer)
  ;; an inner boundary abc.1 is not the outer abc: the outer has two parts, the inner two of its own
  (let* ((text (format nil "Content-Type: multipart/mixed; boundary=abc~%~%--abc~%~
                            Content-Type: multipart/alternative; boundary=abc.1~%~%--abc.1~%~
                            Content-Type: text/plain~%~%plain~%--abc.1~%Content-Type: text/html~%~%<p>html</p>~%~
                            --abc.1--~%--abc  ~%Content-Type: text/plain; name=a.txt~%~%one~%--abc-- ~%"))
         (top (outlook::mime-read (tuatara.text:string-octets text))))
    (is = 2 (length (outlook::mime-part-parts top)))
    (is equal '("multipart/alternative" "text/plain")
        (mapcar #'outlook::mime-part-type (outlook::mime-part-parts top)))
    (is = 2 (length (outlook::mime-part-parts (first (outlook::mime-part-parts top)))))))

(define-test (outlook short-chain-costs-nothing-per-sector)
  ;; a chain of one sector costs the same in a file of a million sectors as in one of ten million
  (let ((table (make-array 1 :initial-element #xFFFFFFFE)))
    (flet ((consed (limit)
             (let ((before (sb-ext:get-bytes-consed)))
               (outlook::cfb-chain 0 table limit)
               (- (sb-ext:get-bytes-consed) before))))
      (consed 10)
      (true (< (consed 1000000) 4096))
      (true (< (consed 10000000) 4096)))))

(define-test (outlook export-name-fits-in-octets)
  ;; a long Cyrillic or Chinese subject is cut to the 255 octets a name may take, its ending kept
  (dolist (letter (list (code-char #x416) (code-char #x4E2D)))
    (let ((name (outlook::mailbox-export-name
                 (outlook::make-mailbox-item :path "/m/Inbox/7" :message t
                                             :sent (encode-universal-time 0 30 10 3 11 2026 0)
                                             :subject (make-string 150 :initial-element letter))
                 "eml")))
      (true (<= (length (tuatara.text:string-octets name)) 255))
      (true (uiop:string-suffix-p name (format nil "~c.eml" letter))))))

(define-test (outlook html-text-costs-little-per-character)
  ;; the text of a page costs about the same per character for a page ten times longer, and little
  (flet ((per-character (count)
           (let* ((html (with-output-to-string (out)
                          (loop repeat count do (write-string "<b>Kea</b>   and <i>tui</i>&amp; ruru<br>" out))))
                  (before (sb-ext:get-bytes-consed)))
             (outlook::html-text html)
             (/ (- (sb-ext:get-bytes-consed) before) (length html)))))
    (per-character 100)
    (let ((small (per-character 2000)) (large (per-character 20000)))
      (format t "~&html-text: ~,1f and ~,1f octets per character~%" small large)
      (true (< small 40))
      (true (< large 40)))))

;;; Outlook mailboxes written, read back by a reader of the tests' own

(defparameter *pst-test-unpermutation*
  (let ((inverse (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 inverse) (setf (aref inverse (aref outlook::*pst-permutation* i)) i)))
  "The inverse of the permutation data blocks are written with: [MS-PST] 5.1's mpbbI.")

(defun pst-test-number (octets at count)
  "The unsigned integer in COUNT octets of OCTETS from AT, least significant first."
  (tuatara:octets-integer octets at count))

(defstruct (pst-test (:constructor %make-pst-test (octets)))
  "A mailbox's OCTETS read by the tests: its NODES (NID to (DATA SUB PARENT)), its BLOCKS (BID to
\(IB . CB)), the 64-octet SLOTS its pages and blocks take, and the PROBLEMS found, in words."
  octets (nodes (make-hash-table)) (blocks (make-hash-table)) (slots (make-hash-table)) (problems '()))

(defun pst-test-problem (pst format &rest arguments)
  "Note a problem of PST, FORMAT with ARGUMENTS."
  (push (apply #'format nil format arguments) (pst-test-problems pst)))

(defun pst-test-take (pst ib size what)
  "Note that SIZE octets at IB of PST are taken by WHAT, a problem when some already were."
  (loop for slot from (floor (- ib #x4400) 64) below (floor (- (+ ib size) #x4400) 64)
        do (when (gethash slot (pst-test-slots pst)) (pst-test-problem pst "~a at ~x overlaps" what ib))
           (setf (gethash slot (pst-test-slots pst)) t)))

(defun pst-test-page (pst ib bid ptype)
  "The page of PST at IB, checked: its type PTYPE, its CRC, its signature and BID."
  (let* ((octets (pst-test-octets pst)) (page (subseq octets ib (+ ib 512))))
    (unless (and (= ptype (aref page 496)) (= ptype (aref page 497))) (pst-test-problem pst "page ~x type" ib))
    (unless (= (pst-test-number page 500 4) (outlook::pst-crc page :end 496)) (pst-test-problem pst "page ~x CRC" ib))
    (unless (= (pst-test-number page 498 2)
               (if (member ptype '(#x82 #x83 #x84 #x85)) 0 (outlook::pst-signature ib bid)))
      (pst-test-problem pst "page ~x signature" ib))
    (unless (= bid (pst-test-number page 504 8)) (pst-test-problem pst "page ~x BID" ib))
    ;; a page's own identifier keeps its lowest bit clear ([MS-PST] 2.2.2.2)
    (when (and (member ptype '(#x80 #x81 #x86)) (logbitp 0 bid)) (pst-test-problem pst "page ~x BID ~x odd" ib bid))
    (pst-test-take pst ib 512 "page")
    page))

(defun pst-test-walk (pst ib bid ptype)
  "Read the B-tree of PST whose root page is at IB with BID, of PTYPE: its nodes or blocks, each
block checked (its trailer, CRC and signature), the keys in order."
  (let* ((page (pst-test-page pst ib bid ptype))
         (count (aref page 488)) (width (aref page 490)) (level (aref page 491)) (keys '()))
    (dotimes (i count)
      (let ((at (* i width)))
        (push (pst-test-number page at 8) keys)
        (cond ((plusp level)
               (pst-test-walk pst (pst-test-number page (+ at 16) 8) (pst-test-number page (+ at 8) 8) ptype))
              ((= ptype #x81)
               (setf (gethash (pst-test-number page at 8) (pst-test-nodes pst))
                     (list (pst-test-number page (+ at 8) 8) (pst-test-number page (+ at 16) 8)
                           (pst-test-number page (+ at 24) 4))))
              (t (let* ((bid (pst-test-number page at 8)) (block-ib (pst-test-number page (+ at 8) 8))
                        (cb (pst-test-number page (+ at 16) 2)) (size (* 64 (ceiling (+ cb 16) 64)))
                        (block (subseq (pst-test-octets pst) block-ib (+ block-ib size))))
                   (setf (gethash bid (pst-test-blocks pst)) (cons block-ib cb))
                   (unless (and (= cb (pst-test-number block (- size 16) 2))
                                (= bid (pst-test-number block (- size 8) 8))
                                (= (pst-test-number block (- size 14) 2) (outlook::pst-signature block-ib bid))
                                (= (pst-test-number block (- size 12) 4) (outlook::pst-crc block :end cb)))
                     (pst-test-problem pst "block ~x trailer" bid))
                   (pst-test-take pst block-ib size "block"))))))
    (unless (equal keys (sort (copy-list keys) #'>)) (pst-test-problem pst "page ~x keys out of order" ib))))

(defun pst-test-read (octets)
  "OCTETS, a Unicode mailbox, read and checked as [MS-PST] says (a PST-TEST): the header's magic,
version and CRCs, both B-trees and every block, each allocation map (AMap) and page map, a free map
where one is due, and the size the header gives."
  (let ((pst (%make-pst-test octets)))
    (unless (tuatara:octets-at-p octets "!BDN" 0) (pst-test-problem pst "no magic"))
    (unless (and (= 23 (pst-test-number octets 10 2)) (= #x80 (aref octets 512)))
      (pst-test-problem pst "not Unicode"))
    (unless (= (pst-test-number octets 4 4) (outlook::pst-crc octets :start 8 :end 479))
      (pst-test-problem pst "partial CRC"))
    (unless (= (pst-test-number octets 524 4) (outlook::pst-crc octets :start 8 :end 524))
      (pst-test-problem pst "full CRC"))
    (unless (= (length octets) (pst-test-number octets 184 8)) (pst-test-problem pst "size"))
    (when (logbitp 0 (pst-test-number octets 32 8)) (pst-test-problem pst "next page's BID odd"))
    (pst-test-walk pst (pst-test-number octets 224 8) (pst-test-number octets 216 8) #x81)
    (pst-test-walk pst (pst-test-number octets 240 8) (pst-test-number octets 232 8) #x80)
    (let ((free 0))
      (loop for start from #x4400 below (length octets) by 253952
            for section from 0
            do (let ((amap (pst-test-page pst start start #x84)))
                 (when (zerop (mod section 8))
                   ;; every page map's bits set, as Outlook writes them
                   (unless (every (lambda (o) (= o #xFF)) (subseq (pst-test-page pst (+ start 512) (+ start 512) #x83) 0 496))
                     (pst-test-problem pst "page map ~d" section)))
                 (when (and (>= section 128) (zerop (mod (- section 128) 496)))
                   (pst-test-page pst (+ start 1024) (+ start 1024) #x82))
                 (dotimes (slot 3968)
                   (let ((bit (ldb (byte 1 (- 7 (mod slot 8))) (aref amap (floor slot 8))))
                         (taken (gethash (+ (* section 3968) slot) (pst-test-slots pst))))
                     (when (zerop bit) (incf free))
                     (unless (eq (= bit 1) (and taken t))
                       (pst-test-problem pst "AMap ~d slot ~d" section slot)
                       (return))))))
      (unless (= (* 64 free) (pst-test-number octets 200 8)) (pst-test-problem pst "free octets")))
    pst))

(defun pst-test-data (pst bid)
  "The data blocks of the data tree BID of PST, a list, each decoded when the mailbox is permuted."
  (destructuring-bind (ib . cb) (gethash (logandc2 bid 1) (pst-test-blocks pst))
    (let ((data (subseq (pst-test-octets pst) ib (+ ib cb))))
      (if (logtest bid 2)
          (loop for i below (pst-test-number data 2 2)
                append (pst-test-data pst (pst-test-number data (+ 8 (* 8 i)) 8)))
          (list (if (= 1 (aref (pst-test-octets pst) 513))
                    (map 'vector (lambda (o) (aref *pst-test-unpermutation* o)) data)
                    data))))))

(defun pst-test-subnodes (pst bid)
  "The subnodes of the subnode tree BID of PST: a table from each NID to (DATA SUB)."
  (let ((table (make-hash-table)))
    (unless (zerop bid)
      (destructuring-bind (ib . cb) (gethash bid (pst-test-blocks pst))
        (let ((block (subseq (pst-test-octets pst) ib (+ ib cb))))
          (dotimes (i (pst-test-number block 2 2))
            (if (zerop (aref block 1))
                (let ((at (+ 8 (* 24 i))))
                  (setf (gethash (pst-test-number block at 8) table)
                        (list (pst-test-number block (+ at 8) 8) (pst-test-number block (+ at 16) 8))))
                (maphash (lambda (k v) (setf (gethash k table) v))
                         (pst-test-subnodes pst (pst-test-number block (+ 16 (* 16 i)) 8))))))))
    table))

(defun pst-test-heap (blocks)
  "The items of the heap on BLOCKS (a node's data blocks), a table from each HID to its octets, and
the user root its header names and its client signature, three values."
  (let ((items (make-hash-table)))
    (loop for block in blocks for k from 0
          do (let* ((map (pst-test-number block 0 2)) (count (pst-test-number block map 2)))
               (dotimes (i count)
                 (setf (gethash (logior (ash (1+ i) 5) (ash k 16)) items)
                       (subseq block (pst-test-number block (+ map 4 (* 2 i)) 2)
                               (pst-test-number block (+ map 6 (* 2 i)) 2))))))
    (values items (pst-test-number (first blocks) 4 4) (aref (first blocks) 3))))

(defun pst-test-bth (items hid)
  "The records of the B-tree on a heap whose header is the item HID of ITEMS, each (KEY . DATA)."
  (let* ((header (gethash hid items)) (key (aref header 1)) (entry (aref header 2)))
    (labels ((records (hid level)
               (let ((data (and (plusp hid) (gethash hid items))))
                 (if (zerop level)
                     (loop for at from 0 below (length data) by (+ key entry)
                           collect (cons (subseq data at (+ at key)) (subseq data (+ at key) (+ at key entry))))
                     (loop for at from 0 below (length data) by (+ key 4)
                           append (records (pst-test-number data (+ at key) 4) (1- level)))))))
      (records (pst-test-number header 4 4) (aref header 3)))))

(defun pst-test-value (pst items subnodes hnid)
  "The value an HNID names: an item of ITEMS, or the data of one of SUBNODES of PST."
  (if (zerop (logand hnid #x1F))
      (gethash hnid items)
      (apply #'concatenate '(vector (unsigned-byte 8)) (pst-test-data pst (first (gethash hnid subnodes))))))

(defun pst-test-pc (pst data sub)
  "The properties of the property context whose data tree is DATA and subnode tree SUB, each (TAG .
OCTETS), a value of four octets or fewer cut to its size."
  (multiple-value-bind (items root signature) (pst-test-heap (pst-test-data pst data))
    (assert (= signature #xBC))
    (let ((subnodes (pst-test-subnodes pst sub)))
      (loop for (key . entry) in (pst-test-bth items root)
            for type = (pst-test-number entry 0 2)
            for size = (outlook::pst-fixed-size type)
            collect (cons (logior (ash (pst-test-number key 0 2) 16) type)
                          (if (and size (<= size 4))
                              (subseq entry 2 (+ 2 size))
                              (pst-test-value pst items subnodes (pst-test-number entry 2 4))))))))

(defun pst-test-tc (pst data sub)
  "The rows of the table context whose data tree is DATA and subnode tree SUB, in the order of their
identifiers, each the properties it holds, (TAG . OCTETS)."
  (multiple-value-bind (items root signature) (pst-test-heap (pst-test-data pst data))
    (assert (= signature #x7C))
    (let* ((info (gethash root items))
           (columns (loop for i below (aref info 1) for at = (+ 22 (* 8 i))
                          collect (list (pst-test-number info at 4) (pst-test-number info (+ at 4) 2)
                                        (aref info (+ at 6)) (aref info (+ at 7)))))
           (size (pst-test-number info 8 2)) (ceb (pst-test-number info 6 2))
           (subnodes (pst-test-subnodes pst sub))
           (rows-hnid (pst-test-number info 14 4))
           (blocks (cond ((zerop rows-hnid) '())
                         ((zerop (logand rows-hnid #x1F)) (list (gethash rows-hnid items)))
                         (t (pst-test-data pst (first (gethash rows-hnid subnodes))))))
           (rows (loop for block in blocks
                       append (loop for at from 0 to (- (length block) size) by size
                                    collect (subseq block at (+ at size))))))
      (loop for (nil . index) in (pst-test-bth items (pst-test-number info 10 4))
            collect (let ((row (nth (pst-test-number index 0 4) rows)))
                      (loop for (tag offset width bit) in columns
                            when (logbitp (- 7 (mod bit 8)) (aref row (+ ceb (floor bit 8))))
                              collect (cons tag
                                            (let ((cell (subseq row offset (+ offset width)))
                                                  (size (outlook::pst-fixed-size (outlook::pst-property-type tag))))
                                              (cond ((and size (<= size 8)) cell)
                                                    (size (gethash (pst-test-number cell 0 4) items))
                                                    (t (pst-test-value pst items subnodes
                                                                       (pst-test-number cell 0 4))))))))))))

(defun pst-test-hex (text)
  "The octets the hexadecimal digits in TEXT stand for, anything else passed over."
  (let ((digits (remove-if-not (lambda (c) (digit-char-p c 16)) text)))
    (coerce (loop for i from 0 below (length digits) by 2
                  collect (parse-integer digits :start i :end (+ i 2) :radix 16))
            '(simple-array (unsigned-byte 8) (*)))))

(define-test (outlook pst-checks-as-the-specification-gives)
  ;; [MS-PST] 3.2's sample header: its two CRCs
  (let ((header (pst-test-hex "21 42 44 4E 0E A9 9A 37 53 4D 17 00 13 00 01 01
               5C 07 00 00 D0 7B 99 0B 04 00 00 00 01 00 00 00
               54 02 00 00 00 00 00 00 45 00 00 00 00 04 00 00
               00 04 00 00 04 04 00 00 00 40 00 00 02 00 01 00
               04 04 00 00 00 04 00 00 00 04 00 00 00 80 00 00
               00 04 00 00 00 04 00 00 00 04 00 00 00 04 00 00
               04 04 00 00 04 04 00 00 04 04 00 00 00 04 00 00
               00 04 00 00 00 04 00 00 00 04 00 00 00 04 00 00
               00 04 00 00 00 04 00 00 00 04 00 00 00 04 00 00
               00 04 00 00 00 04 00 00 00 04 00 00 00 04 00 00
               00 04 00 00 00 04 00 00 0F 04 00 00 00 00 00 00
               00 00 00 00 00 00 00 00 00 24 9F 00 00 00 00 00
               00 44 9B 00 00 00 00 00 40 F2 12 00 00 00 00 00
               00 00 00 00 00 00 00 00 4B 02 00 00 00 00 00 00
               00 52 90 00 00 00 00 00 53 02 00 00 00 00 00 00
               00 0A 90 00 00 00 00 00 02 00 00 00 00 00 00 00")))
    (setf header (concatenate '(vector (unsigned-byte 8)) header (make-array 256 :initial-element #xFF)
                              (pst-test-hex "80 01 00 00 34 14 00 00 00 00 00 00 D6 83 D2 1F")
                              (make-array 36 :initial-element 0)))
    (setf header (coerce header '(simple-array (unsigned-byte 8) (*))))
    (is = 564 (length header))
    (is = #x379AA90E (outlook::pst-crc header :start 8 :end 479))
    (is = #x1FD283D6 (outlook::pst-crc header :start 8 :end 524)))
  ;; the permutation of 5.1 is undone by its inverse, and begins as the table does
  (let ((octets (coerce (loop for i below 512 collect (mod (* i 37) 256)) '(simple-array (unsigned-byte 8) (*)))))
    (is = 65 (aref (outlook::pst-permute (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)) 0))
    (is equalp octets (map 'vector (lambda (o) (aref *pst-test-unpermutation* o)) (outlook::pst-permute octets))))
  ;; 5.5's signature: the halves of the offset and identifier exclusive-ored
  (is = #xDEF0 (outlook::pst-signature #x12345678 #x9ABC))
  ;; 3.8's heap, read by the tests' reader as 3.10 says the message store holds
  (let ((heap (pst-test-hex "EC 00 EC BC 20 00 00 00 00 00 00 00 B5 02 06 00
               40 00 00 00 34 0E 02 01 A0 00 00 00 38 0E 03 00
               00 00 00 00 F9 0F 02 01 60 00 00 00 01 30 1F 00
               80 00 00 00 DF 35 03 00 89 00 00 00 E0 35 02 01
               C0 00 00 00 E3 35 02 01 00 01 00 00 E7 35 02 01
               E0 00 00 00 33 66 0B 00 01 00 00 00 FA 66 03 00
               0D 00 0E 00 FF 67 03 00 00 00 00 00 22 9D B5 0A
               DC D9 94 43 85 DE 90 AE B0 7D 12 70 55 00 4E 00
               49 00 43 00 4F 00 44 00 45 00 31 00 01 00 00 00
               F5 5E F6 66 95 69 CC 4C 83 D1 D8 73 98 99 02 85
               01 00 00 00 00 00 00 00 22 9D B5 0A DC D9 94 43
               85 DE 90 AE B0 7D 12 70 22 80 00 00 00 00 00 00
               22 9D B5 0A DC D9 94 43 85 DE 90 AE B0 7D 12 70
               42 80 00 00 00 00 00 00 22 9D B5 0A DC D9 94 43
               85 DE 90 AE B0 7D 12 70 62 80 00 00 08 00 00 00
               0C 00 14 00 6C 00 7C 00 8C 00 A4 00 BC 00 D4 00
               EC 00")))
    (multiple-value-bind (items root signature) (pst-test-heap (list heap))
      (is = #xBC signature)
      (is = #x20 root)
      (let ((records (pst-test-bth items root)))
        (flet ((value (id)
                 (pst-test-number (cdr (find id records :key (lambda (r) (pst-test-number (car r) 0 2)))) 2 4)))
          (is = 11 (length records))
          (is string= "UNICODE1" (tuatara:utf-16-text (gethash (value #x3001) items)))
          (is = #x89 (value #x35DF))))))
  ;; the same properties written as a property context by the writer, read back alike
  (with-test-directory (dir)
    (let* ((properties (list (outlook::pst-property #x0FF90102
                                                    (pst-test-hex "22 9D B5 0A DC D9 94 43 85 DE 90 AE B0 7D 12 70"))
                             (outlook::pst-property #x3001001F "UNICODE1") (outlook::pst-property #x35DF0003 #x89)
                             (outlook::pst-property #x6633000B t) (outlook::pst-property #x67FF0003 0)
                             (outlook::pst-property #x0E340102 (make-array 5000 :element-type '(unsigned-byte 8)
                                                                                :initial-element 7))))
           (path (namestring (merge-pathnames "one.pst" dir))))
      (tuatara:call-with-free-file (namestring dir) "one.pst"
                                   (lambda (fd path)
                                     (let ((writer (outlook::make-pst-writer fd path)) (subnodes (list '())))
                                       (outlook::pst-node
                                        writer #x21
                                        (outlook::pst-property-context writer properties (outlook::pst-local-nids)
                                                                       subnodes)
                                        (outlook::pst-subnode-tree writer (car subnodes)))
                                       (outlook::pst-finish writer))))
      (let ((pst (pst-test-read (tuatara:file-octets path))))
        (is equal '() (pst-test-problems pst))
        (destructuring-bind (data sub parent) (gethash #x21 (pst-test-nodes pst))
          (declare (ignore parent))
          (is equalp (sort (copy-list properties) #'< :key #'car)
              (sort (pst-test-pc pst data sub) #'< :key #'car)))))))

(defun pst-test-message (i)
  "A message for the tests to write, numbered I: Unicode in its subject, names and text, an HTML
body, two recipients, an attachment of I times 3,000 octets for an even I, a body of more than 3,580
octets for each fifth, and a message attached to the first."
  (outlook::make-pst-message
   :properties (list (outlook::pst-property #x001A001F "IPM.Note")
                     (outlook::pst-property #x0037001F (format nil "Kea survey ~d — ūnga ✓ 😀" i))
                     (outlook::pst-property #x0042001F "Tūī Bird") (outlook::pst-property #x0065001F "tui@example.org")
                     (outlook::pst-property #x0064001F "SMTP")
                     (outlook::pst-property #x00390040
                                            (outlook::pst-filetime (encode-universal-time 0 15 9 30 4 2026 0)))
                     (outlook::pst-property #x0E070003 (if (evenp i) 1 0))
                     (outlook::pst-property #x1000001F
                                            (format nil "Hello ~d~%ūnga~%~a" i
                                                    (if (zerop (mod i 5)) (make-string 4000 :initial-element #\k) "")))
                     (outlook::pst-property #x10130102
                                            (tuatara:string-octets (format nil "<p>Hello <b>~d</b> ūnga</p>" i)))
                     (outlook::pst-property #x007D001F
                                            (format nil "From: Tui <tui@example.org>~%~
                                                         To: Kaka <kaka@example.org>, Second <second@example.org>~%~
                                                         Subject: Kea survey ~d~%Date: Thu, 30 Apr 2026 09:15:00 +0000~%~
                                                         Message-ID: <m~d@example.org>~%"
                                                    i i)))
   :recipients (loop for (name address kind) in '(("Kākā" "kaka@example.org" 1) ("Second" "second@example.org" 2))
                     collect (list (outlook::pst-property #x3001001F name) (outlook::pst-property #x3003001F address)
                                   (outlook::pst-property #x3002001F "SMTP") (outlook::pst-property #x0C150003 kind)))
   :attachments (append
                 (when (evenp i)
                   (list (list (outlook::pst-property #x3707001F (format nil "notes ū ~d.txt" i))
                               (outlook::pst-property #x370E001F "text/plain")
                               (outlook::pst-property
                                #x37010102 (let ((octets (make-array (* 3000 (1+ i)) :element-type '(unsigned-byte 8))))
                                             (dotimes (k (length octets) octets)
                                               (setf (aref octets k) (mod (* k 7) 251))))))))
                 (when (zerop i)
                   (list (list (outlook::pst-property #x3707001F "inner") (outlook::pst-property #x37050003 5)
                               (cons #x3701000D (pst-test-message 1))))))))

(defun pst-test-write (directory name count &key (encoding :permute))
  "Write a mailbox called NAME in DIRECTORY of COUNT messages (PST-TEST-MESSAGE) in Inbox, and the
first message again in Inbox/Birds; return its path."
  (tuatara:call-with-free-file directory name
                               (lambda (fd path)
                                 (outlook::write-pst fd path "Tests"
                                                     (list (cons '("Inbox") (loop for i below count collect i))
                                                           (cons '("Inbox" "Birds") (list 0)))
                                                     :encoding encoding :each #'pst-test-message)
                                 path)))

(define-test (outlook pst-written-and-read-back)
  ;; every layer read back by the tests' reader: the checks, the B-trees of several levels, the
  ;; nodes every mailbox has, the folders, each message's properties, recipients and attachments
  (with-test-directory (dir)
    (dolist (encoding '(:permute :none))
      (let* ((pst (pst-test-read (tuatara:file-octets
                                  (pst-test-write (namestring dir) "kea.pst" 240 :encoding encoding))))
             (nodes (pst-test-nodes pst)))
        (is equal '() (pst-test-problems pst))
        (is = (if (eq encoding :permute) 1 0) (aref (pst-test-octets pst) 513))
        ;; [MS-PST] 2.7.1's nodes
        (dolist (nid '(#x21 #x61 #x122 #x12D #x12E #x12F #x1E1 #x201 #x60D #x60E #x60F #x610 #x692 #x671 #x2223
                       #x8022 #x802D #x802E #x802F #x8042 #x804D #x804E #x804F #x8062 #x806D #x806E #x806F))
          (true (gethash nid nodes) "node ~x" nid))
        (flet ((pc (nid) (destructuring-bind (data sub parent) (gethash nid nodes)
                           (declare (ignore parent))
                           (pst-test-pc pst data sub)))
               (tc (nid) (destructuring-bind (data sub parent) (gethash nid nodes)
                           (declare (ignore parent))
                           (pst-test-tc pst data sub)))
               (text (properties tag) (tuatara:utf-16-text (cdr (assoc tag properties)))))
          ;; the root's two rows of [MS-PST] 2.7.1, the search folder of spam in it but not listed
          (is equal '("Top of Personal Folders" "Search Root")
              (mapcar (lambda (row) (text row #x3001001F)) (tc #x12D)))
          (is equal '("Deleted Items" "Inbox") (mapcar (lambda (row) (text row #x3001001F)) (tc #x802D)))
          (let* ((inbox (pst-test-number (cdr (assoc #x67F20003 (second (tc #x802D)))) 0 4))
                 (rows (tc (logior (logandc2 inbox #x1F) #xE)))
                 (messages (loop for nid being the hash-keys of nodes using (hash-value entry)
                                 when (and (= 4 (logand nid #x1F)) (= (third entry) inbox)) collect nid)))
            (is string= "Tests" (text (pc #x21) #x3001001F))
            (is = 240 (length rows))
            (is = 240 (length messages))
            (is = 240 (pst-test-number (cdr (assoc #x36020003 (pc inbox))) 0 4))
            (is equal '("Birds") (mapcar (lambda (row) (text row #x3001001F)) (tc (logior (logandc2 inbox #x1F) #xD))))
            (loop for nid in (sort messages #'<) for i from 0
                  do (destructuring-bind (data sub parent) (gethash nid nodes)
                       (declare (ignore parent))
                       (let* ((written (pst-test-message i))
                              (properties (pst-test-pc pst data sub))
                              (subnodes (pst-test-subnodes pst sub)))
                         ;; each property as given, but the flags saying it has attachments; those every
                         ;; message has added
                         (loop for (tag . value) in (outlook::pst-message-properties written)
                               unless (or (= tag #x0E070003) (equalp value (cdr (assoc tag properties))))
                                 do (false tag "message ~d property ~x" i tag))
                         (dolist (tag '(#x0E080003 #x0E170003 #x30070040 #x30080040 #x300B0102 #x0E060040))
                           (true (assoc tag properties) "message ~d lacks ~x" i tag))
                         (is = (if (evenp i) #x11 0) (pst-test-number (cdr (assoc #x0E070003 properties)) 0 4))
                         (is equal '("Kākā" "Second")
                             (mapcar (lambda (row) (text row #x3001001F))
                                     (apply #'pst-test-tc pst (gethash #x692 subnodes))))
                         (let ((attachments (loop for k being the hash-keys of subnodes using (hash-value entry)
                                                  when (= 5 (logand k #x1F))
                                                    collect (pst-test-pc pst (first entry) (second entry)))))
                           (is = (+ (if (evenp i) 1 0) (if (zerop i) 1 0)) (length attachments))
                           (when (evenp i)
                             (let ((file (find #x37010102 attachments :key (lambda (a) (car (assoc #x37010102 a))))))
                               (is equalp (cdr (assoc #x37010102 (first (outlook::pst-message-attachments written))))
                                   (cdr (assoc #x37010102 file)))
                               (is = (* 3000 (1+ i)) (pst-test-number (cdr (assoc #x0E200003 file)) 0 4))
                               (is string= (format nil "notes ū ~d.txt" i) (text file #x3707001F)))))))))))))
  ;; past 2 GB nothing is written yet: refused as the place is given out
  (let ((writer (outlook::make-pst-writer nil "none")))
    (setf (outlook::pst-writer-end writer) (- (* 2 1024 1024 1024) 4096))
    (fail (outlook::pst-allocate writer 8192 64))))

(defun pst-test-mailbox (directory pst)
  "The mailbox at PST read for a list as one opened is (readpst writing into DIRECTORY): its list
buffer, rendered, and its job."
  (let* ((list (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "kea.pst (mailbox)")))
         (job (outlook::make-mailbox-job list pst (namestring (ensure-directories-exist directory)))))
    (outlook::mailbox-work job)
    (tuatara.modes:set-major-mode list 'outlook::mailbox-mode)
    (setf (tuatara.buffer:buffer-local list 'outlook::mailbox-path) pst)
    (outlook::mailbox-render list job)
    (values list job)))

(defun pst-test-mail (part)
  "What the tests compare of saved mail PART (MIME-READ): its headers but the content type readpst
writes with a boundary of its own, its plain text, its HTML, and its attachments, each its name,
type and contents, an attached message compared alike."
  (let* ((leaves (outlook::mime-leaves part))
         (body (outlook::mime-body-part leaves))
         (plain (outlook::mime-text-part leaves "text/plain"))
         (html (outlook::mime-text-part leaves "text/html")))
    (list (remove "content-type" (outlook::mime-part-headers part) :key #'car :test #'string=)
          (and plain (outlook::mime-part-text plain))
          (and html (outlook::mime-part-content html))
          (loop for leaf in leaves
                when (outlook::mime-attachment-p leaf body)
                  collect (list (outlook::mime-part-name leaf) (outlook::mime-part-type leaf)
                                (if (outlook::mime-part-message leaf)
                                    (pst-test-mail (outlook::mime-part-message leaf))
                                    (outlook::mime-part-content leaf)))))))

(defun pst-test-messages (job)
  "The messages JOB read, each (FOLDER . WHAT-PST-TEST-MAIL-COMPARES), FOLDER below the top folder."
  (loop for (folder . items) in (outlook::mailbox-job-folders job)
        append (loop for item in items
                     when (outlook::mailbox-item-message item)
                       collect (cons (format nil "~{~a~^/~}" (rest (uiop:split-string folder :separator "/")))
                                     (pst-test-mail (outlook::mime-read
                                                     (tuatara:file-octets (outlook::mailbox-item-path item))))))))

(define-test (outlook pst-copied-out-and-read-back-by-readpst)
  ;; a mailbox read by readpst, its messages written into a new one by C p, and that read by readpst
  ;; again: every message's headers, plain and HTML text and attachments as they were, an attached
  ;; message's too, and by libpff where it is installed; a name taken gets (2); nothing is written
  ;; in an evidence directory
  (if (not (outlook::mailbox-readpst))
      (skip "readpst is not installed here" (true nil))
      (with-test-directory (dir)
        (let ((source (pst-test-write (namestring dir) "kea.pst" 12))
              (copies (namestring (ensure-directories-exist (merge-pathnames "copies/" dir)))))
          (multiple-value-bind (list job) (pst-test-mailbox (merge-pathnames "out/" dir) source)
            (is = 13 (length (pst-test-messages job)))
            (dotimes (round 2)
              (tuatara.tests::run-keys
               (list (lambda () (tuatara:show-buffer list))
                     (format nil "C a p C-a C-k ~a C-j"
                             (tuatara.tests::rimu-typed (concatenate 'string copies "birds")))
                     #'wait-for-mailbox-export :tick)
               :width 140 :height 30))
            (is equal '("birds (2).pst" "birds.pst")
                (sort (mapcar #'first (tuatara.io:directory-entries copies)) #'string<))
            (let ((pst (pst-test-read (tuatara:file-octets (concatenate 'string copies "birds.pst")))))
              (is equal '() (pst-test-problems pst)))
            (multiple-value-bind (back back-job) (pst-test-mailbox (merge-pathnames "back/" dir)
                                                                   (concatenate 'string copies "birds.pst"))
              (let ((before (pst-test-messages job)) (after (pst-test-messages back-job)))
                (is = (length before) (length after))
                (loop for a in before for b in after
                      do (is equalp a b)))
              (tuatara.buffer:kill-buffer back))
            ;; and read by libpff: every message's text and attachments as in the first mailbox
            (let ((before (pst-test-pffexport source (merge-pathnames "pff-source" dir)))
                  (after (pst-test-pffexport (concatenate 'string copies "birds.pst") (merge-pathnames "pff-birds" dir))))
              (if (null before)
                  (skip "libpff's pffexport is not installed here" (true nil))
                  (progn
                    ;; thirteen messages and the one attached to the first, in Inbox and in Birds
                    (is = 15 (count-if (lambda (name) (uiop:string-suffix-p name "Message.txt"))
                                       (alexandria:hash-table-keys before)))
                    (is = (hash-table-count before) (hash-table-count after))
                    (maphash (lambda (name octets) (is equalp octets (gethash name after) "~a" name)) before))))
            ;; an evidence directory is refused, nothing written there
            (let ((evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/" dir)))))
              (with-evidence-directories ((list evidence))
                (let ((export (outlook::make-mailbox-export-job list (outlook::mailbox-export-items list) evidence "pst"
                                                                "kea.pst" (outlook::mailbox-export-folders
                                                                           list (outlook::mailbox-export-items list)))))
                  (fail (outlook::mailbox-export-pst-work export))
                  (is equal '() (tuatara.io:directory-entries evidence)))))
            (tuatara.buffer:kill-buffer list))))))

(defun pst-test-program (name)
  "The full path of the program NAME on the PATH, or NIL: libpff's pffinfo and pffexport, which
the tests use when they are installed."
  (loop for directory in (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":")
        for path = (and (plusp (length directory)) (probe-file (format nil "~a/~a" directory name)))
        when path return (namestring path)))

(defun pst-test-pffinfo (path)
  "What libpff's pffinfo says of the mailbox at PATH, and whether it read it, two values; NIL when
pffinfo is not installed."
  (let ((pffinfo (pst-test-program "pffinfo")))
    (when pffinfo
      (multiple-value-bind (output error code)
          (uiop:run-program (list pffinfo path) :output :string :error-output :string :ignore-error-status t)
        (values (concatenate 'string output error) (zerop code))))))

(defun pst-test-pffexport (path directory)
  "The messages' texts and attachments libpff's pffexport writes of the mailbox at PATH into
DIRECTORY: a table from each file's path below DIRECTORY (Message.txt, its carriage returns left
out, an attached message's too, and Attachments/NAME, an attachment's contents) to its octets; NIL when pffexport is not installed."
  (let ((pffexport (pst-test-program "pffexport")) (files (make-hash-table :test 'equal)))
    (when pffexport
      (uiop:run-program (list pffexport "-q" "-t" (namestring directory) path) :ignore-error-status t)
      (let ((root (namestring (truename (format nil "~a.export/" (string-right-trim "/" (namestring directory)))))))
        (dolist (file (let ((all '()))
                        (uiop:collect-sub*directories root t t
                                                      (lambda (d) (setf all (append (uiop:directory-files d) all))))
                        all)
                      files)
          (let ((name (subseq (namestring file) (length root))))
            (cond ((uiop:string-suffix-p name "/Message.txt")
                   (setf (gethash name files) (remove 13 (tuatara:file-octets (namestring file)))))
                  ((cl-ppcre:scan "/Attachments/[^/]+$" name)
                   (setf (gethash name files) (tuatara:file-octets (namestring file)))))))))))

(define-test (outlook pst-name-map-read-by-libpff)
  ;; the map of named properties holds entries, a GUID and a string, and the hash buckets: libpff
  ;; refuses a mailbox whose map has an empty stream
  (with-test-directory (dir)
    (let* ((path (pst-test-write (namestring dir) "map.pst" 2))
           (pst (pst-test-read (tuatara:file-octets path))))
      (is equal '() (pst-test-problems pst))
      (destructuring-bind (data sub parent) (gethash #x61 (pst-test-nodes pst))
        (declare (ignore parent))
        (let* ((map (pst-test-pc pst data sub))
               (entries (cdr (assoc #x00030102 map))))
          (is = 251 (pst-test-number (cdr (assoc #x00010003 map)) 0 4))
          (true (and entries (plusp (length entries)) (zerop (mod (length entries) 8))))
          ;; each entry named by a number is in its bucket (one named by a string by the string's CRC)
          (loop for at from 0 below (length entries) by 8
                for bucket = (mod (logxor (pst-test-number entries at 4) (pst-test-number entries (+ at 4) 2)) 251)
                unless (logtest 1 (aref entries (+ at 4)))
                  do (true (assoc (logior (ash (+ #x1000 bucket) 16) #x0102) map) "bucket ~d" bucket))
          (true (plusp (length (cdr (assoc #x00020102 map)))))
          (true (plusp (length (cdr (assoc #x00040102 map)))))))
      (multiple-value-bind (said read) (pst-test-pffinfo path)
        (if said
            (true read "pffinfo: ~a" said)
            (skip "libpff's pffinfo is not installed here" (true nil)))))))

(define-test (outlook pst-heap-of-many-items)
  ;; a block of a heap holds at most 2,047 items, the most an index of 11 bits numbers: a property
  ;; context of 2,500 empty values reads back whole
  (with-test-directory (dir)
    (let ((properties (loop for id from #x6000 below (+ #x6000 2500)
                            collect (outlook::pst-property (logior (ash id 16) #x0102) (outlook::pst-octets 0))))
          (path (namestring (merge-pathnames "many.pst" dir))))
      (tuatara:call-with-free-file (namestring dir) "many.pst"
                                   (lambda (fd path)
                                     (let ((writer (outlook::make-pst-writer fd path)) (subnodes (list '())))
                                       (outlook::pst-node writer #x21
                                                          (outlook::pst-property-context
                                                           writer properties (outlook::pst-local-nids) subnodes)
                                                          0)
                                       (outlook::pst-finish writer))))
      (let ((pst (pst-test-read (tuatara:file-octets path))))
        (is equal '() (pst-test-problems pst))
        (destructuring-bind (data sub parent) (gethash #x21 (pst-test-nodes pst))
          (declare (ignore parent))
          (is equalp properties (sort (pst-test-pc pst data sub) #'< :key #'car)))))))

;;; The label manager

(defun manager-test-mailbox (dir name sum labelled)
  "The list and job of the mailbox of *MAILBOX-LABEL-TEST-MESSAGES* read in
the directory NAME of DIR with the SHA-256 sum SUM, its messages given the
labels LABELLED says, each (SUBJECT LABEL ...), and kept."
  (multiple-value-bind (list job) (mailbox-test-list (merge-pathnames name dir) *mailbox-label-test-messages* :sum sum)
    (loop for (subject . labels) in labelled
          do (let ((item (find subject (loop for (nil . items) in (outlook::mailbox-job-folders job) append items)
                               :key #'outlook::mailbox-item-subject :test #'equal)))
               (outlook::mailbox-relabel job (list item) (lambda (had) (append had labels)))
               (wait-for-mailbox-labels)))
    (push list outlook::*mailboxes*)
    (values list job)))

(defun manager-test-sum ()
  "A new SHA-256 sum for a mailbox of a test: 64 lowercase hexadecimal digits."
  (format nil "~(~64,'0x~)" (random (expt 2 256))))

(defun manager-test-open ()
  "Open the label manager, as the command does, and wait for it to read the
labels; its buffer."
  (tuatara.tests::run-keys (list (lambda () (outlook::outlook-label-manager)) #'wait-for-mailbox-labels :tick)
                           :width 140 :height 40)
  (tuatara.buffer:find-buffer "*Mailbox labels*"))

(defun manager-test-journal (sum)
  "The table of labels the journal of the mailbox SUM leaves."
  (outlook::mailbox-read-labels (outlook::mailbox-labels-name sum)))

(defun manager-test-labels (job sum)
  "The labels each message of the mailbox JOB read has in the journal of SUM
as it is now, a list for each message in order, read afresh."
  (let ((table (manager-test-journal sum)))
    (mapcar (lambda (item) (gethash (outlook::mailbox-item-identity item) table))
            (cdr (first (outlook::mailbox-job-folders job))))))

(defun manager-test-go (manager sum text)
  "A step for RUN-KEYS: point on the first line of MANAGER showing TEXT after
the line of the mailbox whose sum is SUM (TEXT NIL: on that line itself)."
  (lambda ()
    (let* ((lines (uiop:split-string (tuatara.buffer:buffer-string manager) :separator '(#\Newline)))
           (section (position-if (lambda (line) (search (subseq sum 0 8) line)) lines))
           (line (if text (position-if (lambda (line) (search text line)) lines :start section) section)))
      (setf (tuatara.buffer:buffer-point manager) (tuatara.buffer:buffer-goto-line manager line)))))

(defun manager-test-keys (manager &rest steps)
  "Run STEPS (RUN-KEYS' strings and functions) in a session showing MANAGER,
then wait for the labels job they start and take it in."
  (tuatara.tests::run-keys (append (list (lambda () (tuatara:show-buffer manager))) steps
                                   (list #'wait-for-mailbox-labels :tick))
                           :width 140 :height 40))

(defun manager-test-refresh (manager)
  "Read the journals and show them in MANAGER, as a job in the background does."
  (setf outlook::*mailbox-label-sets* (outlook::mailbox-label-sets))
  (outlook::mailbox-labels-render manager :keep))

(defun manager-test-count (sum label)
  "How many messages of the mailbox SUM carry LABEL, in its journal."
  (loop for labels being the hash-values of (manager-test-journal sum) count (member label labels :test #'string=)))

(defparameter *manager-test-labelled* '(("First" "mgrzeta" "mgrkea") ("Second" "mgrzeta"))
  "The labels the mailboxes of the manager's tests start with.")

(define-test (outlook label-manager-lists-mailboxes)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)))
      (tuatara.buffer:kill-buffer (manager-test-mailbox dir "one/" sum *manager-test-labelled*))
      (let* ((manager (manager-test-open))
             (text (substitute #\Space #\Newline (tuatara.buffer:buffer-string manager)))
             (line (mailbox-test-subject-line manager (subseq sum 0 8))))
        ;; where they are kept and how much room, then the mailbox's file name, sum and counts
        (has text "Labels are kept in ")
        (has text "outlook/labels/")
        (has text "outlook-label-manager")
        (has text "taking ")
        (has line "archive.pst")
        (has line "2 labels on 2 messages")
        (has line "there")
        (has (mailbox-test-subject-line manager "mgrzeta") "2")
        (has (mailbox-test-subject-line manager "mgrkea") "1")
        ;; a file in the directory that is no mailbox's journal is not listed
        (dolist (name (list "names" "x.tmp" (format nil "~a.lock" sum) "abc"))
          (tuatara.io:append-to-file (tuatara.io:path-join (outlook::mailbox-label-directory) name)
                                     (format nil "+~cmgrodd~cX~cY~cZ~%" #\Tab #\Tab #\Tab #\Tab)))
        (manager-test-refresh manager)
        (false (search "mgrodd" (tuatara.buffer:buffer-string manager)))
        (tuatara.buffer:kill-buffer manager)))))

(define-test (outlook mailbox-list-says-where-labels-are-kept)
  (with-test-directory (dir)
    (multiple-value-bind (list) (mailbox-test-list dir *mailbox-label-test-messages*)
      (let ((text (substitute #\Space #\Newline (tuatara.buffer:buffer-string list))))
        (has text (format nil "Labels are kept in ~a (M-x outlook-label-manager)"
                          (tuatara:abbreviated-path (outlook::mailbox-label-directory)))))
      ;; filled to the help column
      (let* ((lines (uiop:split-string (tuatara.buffer:buffer-string list) :separator '(#\Newline)))
             (start (position-if (lambda (line) (search "Labels are kept" line)) lines))
             (paragraph (subseq lines start (position "" lines :start start :test #'string=))))
        (true (rest paragraph))
        (true (every (lambda (line) (<= (length line) tuatara:*help-column*)) paragraph))))))

(define-test (outlook label-manager-renames-and-deletes)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)) (other (manager-test-sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (tuatara.buffer:kill-buffer (manager-test-mailbox dir "two/" other '(("First" "mgrzeta"))))
        (let ((manager (manager-test-open)))
          ;; R: rename a label of this mailbox only, on every message that has it; the list open shows it
          (manager-test-keys manager (manager-test-go manager sum "mgrkea") "R C-a C-k m g r t u i RET")
          (is equal '(("mgrzeta" "mgrtui") ("mgrzeta") nil) (manager-test-labels job sum))
          (is = 1 (manager-test-count other "mgrzeta"))
          (has (mailbox-test-subject-line list "First") "mgrtui")
          (false (search "mgrkea" (mailbox-test-subject-line list "First")))
          (has (mailbox-test-subject-line manager "mgrtui") "1")
          ;; renamed to a label a message has: it only loses the old
          (outlook::mailbox-change-labels (outlook::mailbox-labels-name sum)
                                          (lambda (table)
                                            (outlook::mailbox-rename-label-lines table "mgrtui" "mgrzeta")))
          (is equal '(("mgrzeta") ("mgrzeta") nil) (manager-test-labels job sum))
          (manager-test-refresh manager)
          ;; d marks, x takes the label off every message of its mailbox, after asking
          (manager-test-keys manager (manager-test-go manager sum "mgrzeta") "d x n o RET")
          (is equal '(("mgrzeta") ("mgrzeta") nil) (manager-test-labels job sum))
          (manager-test-keys manager (manager-test-go manager sum "mgrzeta") "d x y e s RET")
          (is equal '(nil nil nil) (manager-test-labels job sum))
          (is = 1 (manager-test-count other "mgrzeta"))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-forgets-and-opens)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)) (other (manager-test-sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (tuatara.buffer:kill-buffer (manager-test-mailbox dir "two/" other '(("First" "mgrzeta"))))
        (let ((manager (manager-test-open))
              (file (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
          ;; D asks, saying how many labels and messages; no leaves everything
          (manager-test-keys manager (manager-test-go manager sum nil) "D")
          (true (probe-file file))
          (manager-test-keys manager (manager-test-go manager sum nil) "D n o RET")
          (true (probe-file file))
          ;; yes forgets that mailbox's labels alone; the list open shows none, the other mailbox keeps its own
          (manager-test-keys manager (manager-test-go manager sum nil) "D y e s RET")
          (false (probe-file file))
          ;; the lock file stays: sessions waiting for it hold the same one
          (true (probe-file (concatenate 'string file ".lock")))
          (is = 0 (hash-table-count (outlook::mailbox-job-labels job)))
          (is = 1 (manager-test-count other "mgrzeta"))
          (false (search (subseq sum 0 8) (tuatara.buffer:buffer-string manager)))
          (let ((path (namestring (merge-pathnames "two/archive.pst" dir))))
            (is equal path (second (outlook::mailbox-journal-meta
                                    (outlook::mailbox-journal-lines
                                     (tuatara.io:path-join (outlook::mailbox-label-directory)
                                                           (outlook::mailbox-labels-name other)))))))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(defun manager-test-slow-writer (list job journal seconds)
  "Make the mailbox JOB of LIST have a job keeping labels that ends after
SECONDS, having added a line to the journal named JOURNAL as such a job does."
  (setf (outlook::mailbox-job-labels-writer job)
        (tuatara.runtime:start-job-thread
         (outlook::make-mailbox-labels-job list journal nil)
         (lambda (writer)
           (declare (ignore writer))
           (sleep seconds)
           (let ((path (tuatara.io:path-join (outlook::mailbox-label-directory) journal)))
             (outlook::mailbox-append-lines path (list (outlook::mailbox-label-line "+" "late" "X")))))
         :name "tuatara test writer")))

(define-test (outlook label-manager-waits-for-writers-before-forgetting)
  (with-test-directory (dir)
    (let* ((sum (manager-test-sum)) (journal (outlook::mailbox-labels-name sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (let ((manager (manager-test-open))
              (file (tuatara.io:path-join (outlook::mailbox-label-directory) journal)))
          ;; lines not yet kept are dropped, and a writer under way ends before the journal goes
          (setf (outlook::mailbox-job-labels-pending job) (list (outlook::mailbox-label-line "+" "pending" "X")))
          (manager-test-slow-writer list job journal 0.5)
          (manager-test-keys manager (manager-test-go manager sum nil) "D y e s RET")
          (false (outlook::mailbox-job-labels-pending job))
          (false (probe-file file))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-leaves-mailboxes-being-written)
  (with-test-directory (dir)
    (let* ((sum (manager-test-sum)) (journal (outlook::mailbox-labels-name sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (let ((manager (manager-test-open)) (first (first (cdr (first (outlook::mailbox-job-folders job))))))
          ;; a journal changed meanwhile is not shown while the mailbox's own changes are being kept
          (outlook::mailbox-change-labels journal (lambda (table) (declare (ignore table))
                                                    (list (outlook::mailbox-label-line
                                                           "+" "other session" (outlook::mailbox-item-identity first)))))
          (manager-test-slow-writer list job journal 0.5)
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer manager))
                 (lambda () (outlook::mailbox-labels-start manager "reading" nil))
                 (lambda () (loop while (outlook::mailbox-labels-working manager) do (sleep 0.01)))
                 :tick)
           :width 140 :height 40)
          (is equal '("mgrzeta" "mgrkea") (outlook::mailbox-item-labels job first))
          ;; and shown once they are
          (let ((until (+ (get-internal-real-time) (* 5 internal-time-units-per-second))))
            (loop while (and (tuatara.runtime:job-running-p (outlook::mailbox-job-labels-writer job))
                             (< (get-internal-real-time) until))
                  do (sleep 0.01)))
          (manager-test-keys manager (lambda () (outlook::mailbox-labels-start manager "reading" nil)))
          (true (member "other session" (outlook::mailbox-item-labels job first) :test #'string=))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-shows-what-it-has-read)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)))
      (tuatara.buffer:kill-buffer (manager-test-mailbox dir "one/" sum '(("First" "cachelabel"))))
      (let ((manager (manager-test-open))
            (file (tuatara.io:path-join (outlook::mailbox-label-directory) (outlook::mailbox-labels-name sum))))
        ;; showing, marking and taking the list in again read no journal: the cache is what is shown
        (delete-file file)
        (outlook::mailbox-labels-render manager :keep)
        (has (mailbox-test-subject-line manager "cachelabel") "1")
        ;; work is done in the background, said in the heading, and one at a time
        (let ((release (sb-thread:make-semaphore)))
          (true (outlook::mailbox-labels-start manager "renaming"
                                               (lambda () (sb-thread:wait-on-semaphore release) "done")))
          (has (tuatara.buffer:buffer-string manager) "(renaming...)")
          (let ((working (outlook::mailbox-labels-working manager)))
            (outlook::mailbox-labels-start manager "forgetting" nil)
            (is eq working (outlook::mailbox-labels-working manager)))
          (sb-thread:signal-semaphore release)
          (manager-test-keys manager)
          (false (search "(renaming...)" (tuatara.buffer:buffer-string manager)))
          (false (mailbox-test-subject-line manager "cachelabel")))
        (tuatara.buffer:kill-buffer manager)))))

(define-test (outlook label-manager-copies-labels-out-and-in)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)) (other (manager-test-sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (tuatara.buffer:kill-buffer (manager-test-mailbox dir "two/" other '(("Third" "mgrtui"))))
        (let* ((manager (manager-test-open))
               (out (namestring (merge-pathnames "copy/" dir)))
               (file (format nil "~alabels.txt" out)))
          (ensure-directories-exist out)
          ;; E writes a plain file: the sum, the mailbox's file name, each message's identity and labels;
          ;; a name taken gets (2)
          (dotimes (n 2)
            (manager-test-keys manager (manager-test-go manager sum nil)
                               (format nil "E C-a C-k ~a C-j" (tuatara.tests::rimu-typed file))))
          (is equal '("labels (2).txt" "labels.txt")
              (sort (mapcar #'first (tuatara.io:directory-entries out)) #'string<))
          (let ((text (uiop:read-file-string file)))
            (is string= (format nil "# tuatara-outlook-labels 1~%# sha256 ~a~%# file archive.pst~%~
                                     # deleted-items ~:[no~;yes~]~%"
                                sum tuatara-user:*outlook-include-deleted*)
                (subseq text 0 (search (format nil "+~c" #\Tab) text)))
            (has text (format nil "+~cmgrkea~cOutlook Data File/Inbox~c<one@example.org>~c" #\Tab #\Tab #\Tab #\Tab))
            (is = 3 (count #\+ text)))
          ;; the labels forgotten, then I reads them back; those in the mailbox already stay, others are added
          (manager-test-keys manager (manager-test-go manager sum nil) "D y e s RET")
          (is = 0 (hash-table-count (manager-test-journal sum)))
          (outlook::mailbox-change-labels
           (outlook::mailbox-labels-name sum)
           (lambda (table)
             (declare (ignore table))
             (list (outlook::mailbox-label-line
                    "+" "mgrzeta"
                    (outlook::mailbox-item-identity (second (cdr (first (outlook::mailbox-job-folders job)))))))))
          (manager-test-refresh manager)
          (manager-test-keys manager (manager-test-go manager sum nil)
                             (format nil "I C-a C-k ~a C-j" (tuatara.tests::rimu-typed file)))
          (is equal '(("mgrzeta" "mgrkea") ("mgrzeta") nil) (manager-test-labels job sum))
          (manager-test-keys manager (manager-test-go manager sum nil)
                             (format nil "I C-a C-k ~a C-j" (tuatara.tests::rimu-typed file)))
          (is equal '(("mgrzeta" "mgrkea") ("mgrzeta") nil) (manager-test-labels job sum))
          ;; on another mailbox's line, a file of this one is refused and nothing changes
          (manager-test-keys manager (manager-test-go manager other nil)
                             (format nil "I C-a C-k ~a C-j" (tuatara.tests::rimu-typed file)))
          (is = 1 (manager-test-count other "mgrtui"))
          (is = 0 (manager-test-count other "mgrzeta"))
          ;; anywhere but a mailbox's line it goes to the mailbox it is of, which may have been forgotten
          (manager-test-keys manager (manager-test-go manager sum nil) "D y e s RET")
          (manager-test-keys manager (lambda () (setf (tuatara.buffer:buffer-point manager) 0))
                             (format nil "I C-a C-k ~a C-j" (tuatara.tests::rimu-typed file)))
          (is equal '(("mgrzeta" "mgrkea") ("mgrzeta") nil) (manager-test-labels job sum))
          ;; the mailbox list's keys: M and C-c o m open the manager
          (dolist (key '("M" "C-c o m"))
            (tuatara.buffer:kill-buffer manager)
            (tuatara.tests::run-keys (list (lambda () (tuatara:show-buffer list)) key) :width 140 :height 40)
            (true (tuatara.buffer:find-buffer "*Mailbox labels*"))
            (setf manager (tuatara.buffer:find-buffer "*Mailbox labels*")))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-refuses-files-not-of-labels)
  (with-test-directory (dir)
    (let ((good (manager-test-sum)))
      (flet ((file-with (sum &optional (name "archive.pst"))
               (let ((path (namestring (merge-pathnames (format nil "f~d.txt" (random 100000)) dir))))
                 (put-test-file path (format nil "# tuatara-outlook-labels 1~%# sha256 ~a~%# file ~a~%# deleted-items no~%~
                                                  +~ca~cb~cc~cd~%" sum name #\Tab #\Tab #\Tab #\Tab))
                 path)))
        ;; the sum is the journal's file name: exactly 64 lowercase hexadecimal digits, nothing else
        (is equal good (outlook::mailbox-read-labels-file (file-with good)))
        (dolist (bad (list "." ".." "names" "abc.lock" "x.tmp" (make-string 300 :initial-element #\a)
                           (format nil "~a~c~a" (subseq good 0 30) (code-char 0) (subseq good 31))
                           (string-upcase good) (subseq good 1) (format nil "~a.with-deleted" good) "../etc"))
          (false (outlook::mailbox-read-labels-file (file-with bad))))
        ;; the file name is kept short and without control characters
        (let ((name (nth-value 1 (outlook::mailbox-read-labels-file
                                  (file-with good (format nil "a~cb~a" (code-char 7) (make-string 400 :initial-element #\x)))))))
          (is = 255 (length name))
          (false (find-if (lambda (c) (< (char-code c) 32)) name)))
        ;; a file of other text is refused
        (let ((bad (format nil "~abad.txt" (namestring dir))))
          (put-test-file bad (format nil "just some text~%"))
          (false (outlook::mailbox-read-labels-file bad)))))))

(define-test (outlook label-manager-refuses-while-busy-and-forgetting)
  (with-test-directory (dir)
    (let* ((sum (manager-test-sum)) (journal (outlook::mailbox-labels-name sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum *manager-test-labelled*)
        (let ((manager (manager-test-open)) (release (sb-thread:make-semaphore))
              (line (format nil "+~cpending~cX~cY~cZ~%" #\Tab #\Tab #\Tab #\Tab)))
          ;; with a job under way, D says to wait and the lines not yet kept stay
          (setf (outlook::mailbox-job-labels-pending job) (list line))
          (outlook::mailbox-labels-start manager "working" (lambda () (sb-thread:wait-on-semaphore release) nil))
          (tuatara.tests::run-keys
           (list (lambda () (tuatara:show-buffer manager)) (manager-test-go manager sum nil)
                 (lambda () (outlook::outlook-labels-forget))
                 (lambda () (sb-thread:signal-semaphore release))
                 #'wait-for-mailbox-labels :tick)
           :width 140 :height 40)
          (is equal (list line) (outlook::mailbox-job-labels-pending job))
          (true (probe-file (tuatara.io:path-join (outlook::mailbox-label-directory) journal)))
          (setf (outlook::mailbox-job-labels-pending job) nil)
          ;; no label is put on a mailbox while its labels are being forgotten
          (let ((outlook::*mailbox-labels-forgetting* (list journal)))
            (tuatara.tests::run-keys
             (list (lambda () (tuatara:show-buffer list)) (mailbox-test-go-to list "First")
                   (lambda () (false (outlook::mailbox-labelled-items))))
             :width 140 :height 30))
          (true (tuatara.tests::run-keys
                 (list (lambda () (tuatara:show-buffer list)) (mailbox-test-go-to list "First")
                       (lambda () (true (outlook::mailbox-labelled-items))))
                 :width 140 :height 30))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-says-what-went-wrong-and-reads-again)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)))
      (tuatara.buffer:kill-buffer (manager-test-mailbox dir "one/" sum '(("First" "halflabel"))))
      (let ((manager (manager-test-open)) (journal (outlook::mailbox-labels-name sum)) (working nil))
        ;; an action that fails part way: what it did is shown, the failure said
        (tuatara.tests::run-keys
         (list (lambda () (tuatara:show-buffer manager))
               (lambda ()
                 (outlook::mailbox-labels-start
                  manager "changing"
                  (lambda ()
                    (outlook::mailbox-change-labels
                     journal (lambda (table) (declare (ignore table))
                               (list (outlook::mailbox-label-line
                                      "+" "extra" (format nil "Inbox~c~c~a" #\Tab #\Tab sum)))))
                    (error "disk full"))))
               (lambda () (setf working (outlook::mailbox-labels-working manager)))
               #'wait-for-mailbox-labels)
         :width 140 :height 30)
        (has (outlook::mailbox-labels-manager-job-result working) "Not all were changed: ")
        (has (outlook::mailbox-labels-manager-job-result working) "disk full")
        (tuatara.tests::run-keys (list (lambda () (tuatara:show-buffer manager)) :tick) :width 140 :height 30)
        (has (tuatara.buffer:buffer-string manager) "extra")
        (tuatara.buffer:kill-buffer manager)))))

(define-test (outlook label-manager-keeps-labels-given-during-a-job)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)))
      (multiple-value-bind (list job) (manager-test-mailbox dir "one/" sum '(("First" "before")))
        (let ((manager (manager-test-open)) (first (first (cdr (first (outlook::mailbox-job-folders job))))))
          ;; a job reads the labels, then one is given and kept before the job is taken in: not lost from the list
          (outlook::mailbox-labels-start manager "reading" nil)
          (loop while (outlook::mailbox-labels-working manager) do (sleep 0.01))
          (outlook::mailbox-relabel job (list first) (lambda (had) (append had '("fresh"))))
          (wait-for-mailbox-labels)
          (manager-test-keys manager)
          (is equal '("before" "fresh") (outlook::mailbox-item-labels job first))
          (tuatara.buffer:kill-buffer manager)
          (tuatara.buffer:kill-buffer list))))))

(define-test (outlook label-manager-says-when-still-reading-or-empty)
  (with-test-directory (dir)
    (let ((sum (manager-test-sum)) (release (sb-thread:make-semaphore)))
      (tuatara.buffer:kill-buffer (manager-test-mailbox dir "one/" sum '(("First" "somelabel"))))
      (let ((manager (manager-test-open)) (outlook::*mailbox-label-sets* nil))
        ;; the first read not ended: said so, not 0 mailboxes
        (outlook::mailbox-labels-start manager "reading the labels"
                                       (lambda () (sb-thread:wait-on-semaphore release) nil))
        (has (tuatara.buffer:buffer-string manager) "still reading")
        (false (search "0 mailboxes" (tuatara.buffer:buffer-string manager)))
        (sb-thread:signal-semaphore release)
        (manager-test-keys manager)
        ;; a mailbox with no labels is not written out
        (let* ((set (outlook::mailbox-label-set-named (outlook::mailbox-labels-name sum)))
               (out (namestring (merge-pathnames "empty.labels" dir))))
          (declare (ignore set))
          (outlook::mailbox-change-labels (outlook::mailbox-labels-name sum)
                                          (lambda (table)
                                            (loop for identity being the hash-keys of table using (hash-value labels)
                                                  append (loop for label in labels
                                                               collect (outlook::mailbox-label-line
                                                                        "-" label identity)))))
          (manager-test-refresh manager)
          (let ((empty (outlook::mailbox-read-label-set (outlook::mailbox-labels-name sum) 0)))
            (has (outlook::mailbox-export-labels empty out) "no labels to write")
            (false (probe-file out))))
        (tuatara.buffer:kill-buffer manager)))))

(define-test (outlook label-manager-lets-go-of-a-mailbox-forgotten)
  (with-test-directory (dir)
    (let* ((sum (manager-test-sum)) (journal (outlook::mailbox-labels-name sum)) (release (sb-thread:make-semaphore)))
      (tuatara.buffer:kill-buffer (manager-test-mailbox dir "one/" sum *manager-test-labelled*))
      (let ((manager (manager-test-open)))
        ;; not kept from labelling when the job is refused
        (outlook::mailbox-labels-start manager "working" (lambda () (sb-thread:wait-on-semaphore release) nil))
        (outlook::mailbox-labels-start manager "forgetting" nil :forgetting journal)
        (false (member journal outlook::*mailbox-labels-forgetting* :test #'string=))
        (sb-thread:signal-semaphore release)
        (manager-test-keys manager)
        ;; kept while the job is under way, let go once it is taken in, whatever its action did
        (outlook::mailbox-labels-start manager "forgetting" (lambda () nil) :forgetting journal)
        (true (member journal outlook::*mailbox-labels-forgetting* :test #'string=))
        (manager-test-keys manager)
        (false (member journal outlook::*mailbox-labels-forgetting* :test #'string=))
        (tuatara.buffer:kill-buffer manager)))))
