;;;; msg.lisp --- Outlook items (.msg) read: their properties, recipients and attachments.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; An Outlook item is a compound file (cfb.lisp) laid out as Microsoft's
;;;; MS-OXMSG says: the item's properties, each a number and a type, those of
;;;; a fixed size in one stream (__properties_version1.0) and each of the
;;;; others in a stream of its own (__substg1.0_NNNNTTTT); a storage for each
;;;; recipient and each attachment, laid out alike; an attachment that is an
;;;; item itself, a storage within, read as one to a depth. An item is read
;;;; too as a message to be written in a new mailbox (pst.lisp), its
;;;; properties as they are stored, its named properties (those numbered from
;;;; 0x8000) with what its map of them says they stand for. A message of a
;;;; mailbox is written as an Outlook item too, for the list of the mailbox
;;;; (items.lisp).

(in-package #:outlook)

(defun msg-stored-properties (cfb entry header)
  "The properties of the storage ENTRY of the compound file (CFB), an item, a
recipient or an attachment, whose fixed-size properties' stream has HEADER
octets before them, as they are stored: a list of (TAG . VALUE), TAG the
property's number and type, VALUE the eight octets of a fixed-size one, the
octets of a string, binary data or a GUID as its stream holds them, or
\(:storage . ENTRY) for an object. Multiple values (streams of their own,
-NNNNNNNN after) are left out."
  (let ((properties '()))
    (dolist (child (cfb-children cfb entry) (nreverse properties))
      (let ((name (cfb-entry-name child)))
        (cond
          ((and (eq (cfb-entry-type child) :stream) (string= name "__properties_version1.0"))
           (let ((octets (cfb-stream-octets cfb child)))
             (loop for at from header by 16
                   while (<= (+ at 16) (length octets))
                   do (let ((tag (cfb-number octets at 4)))
                        (when (member (ldb (byte 16 0) tag)
                                      '(#x0002 #x0003 #x0004 #x0005 #x0006 #x0007 #x000A #x000B #x0014 #x0040))
                          (push (cons tag (subseq octets (+ at 8) (+ at 16))) properties))))))
          ((and (= (length name) 20) (string= "__substg1.0_" name :end2 12))
           (let ((id (ignore-errors (parse-integer name :start 12 :end 16 :radix 16)))
                 (type (ignore-errors (parse-integer name :start 16 :end 20 :radix 16))))
             (when (and id type)
               (case type
                 ((#x001F #x001E #x0102 #x0048)
                  (when (eq (cfb-entry-type child) :stream)
                    (push (cons (logior (ash id 16) type) (cfb-stream-octets cfb child)) properties)))
                 (#x000D (push (cons (logior (ash id 16) type) (cons :storage child)) properties)))))))))))

(defun msg-properties (cfb entry header)
  "The properties of the storage ENTRY of the compound file (CFB), an item, a
recipient or an attachment, whose fixed-size properties' stream has HEADER
octets before them (MSG-STORED-PROPERTIES): a hash table from each property's
number to (TYPE . VALUE), VALUE a number, a universal time, T or NIL, octets,
a string, or (:storage . ENTRY)."
  (let ((table (make-hash-table)))
    (loop for (tag . raw) in (msg-stored-properties cfb entry header)
          for type = (ldb (byte 16 0) tag)
          for id = (ldb (byte 16 16) tag)
          for old = (car (gethash id table))
          do (case type
               (#x0002 (setf (gethash id table) (cons type (octets-signed (cfb-number raw 0 2) 16))))
               (#x0003 (setf (gethash id table) (cons type (octets-signed (cfb-number raw 0 4) 32))))
               (#x000B (setf (gethash id table) (cons type (/= 0 (cfb-number raw 0 2)))))
               (#x0014 (setf (gethash id table) (cons type (cfb-number raw 0 8))))
               (#x0040 (setf (gethash id table) (cons type (filetime-universal-time (cfb-number raw 0 8)))))
               ;; a string of Unicode before one of the item's code page
               ((#x001F #x001E #x0102)
                (unless (and (eql old #x001F) (/= type #x001F))
                  (setf (gethash id table)
                        (cons type (if (= type #x001F)
                                       (let ((text (pst-utf-16-text raw)))
                                         (subseq text 0 (or (position (code-char 0) text) (length text))))
                                       raw)))))
               (#x000D (setf (gethash id table) (cons type raw)))))
    table))

(defun msg-value (properties id)
  "The value of the property numbered identifier (ID) in PROPERTIES
\(MSG-PROPERTIES), or NIL."
  (cdr (gethash id properties)))

(defun msg-string (properties id codepage)
  "The property numbered identifier (ID) in PROPERTIES as text: a string as it
is, one in the item's code page (CODEPAGE, a number) decoded, a number written;
NIL for none or an empty one."
  (let* ((entry (gethash id properties))
         (value (cond ((null entry) nil)
                      ((stringp (cdr entry)) (cdr entry))
                      ((and (= (car entry) #x001E) (vectorp (cdr entry)))
                       (let ((text (charset-text (cdr entry) (or (mail-codepage-charset codepage) "windows-1252"))))
                         (subseq text 0 (or (position (code-char 0) text) (length text)))))
                      ((integerp (cdr entry)) (princ-to-string (cdr entry))))))
    (and value (plusp (length value)) value)))

(defun msg-children-named (cfb entry prefix)
  "The storages in ENTRY of the compound file (CFB) whose names begin with
PREFIX, in the order of their names."
  (sort (remove-if-not (lambda (child) (and (eq (cfb-entry-type child) :storage)
                                            (uiop:string-prefix-p prefix (cfb-entry-name child))))
                       (cfb-children cfb entry))
        #'string< :key #'cfb-entry-name))

(defparameter *msg-flags*
  '((#x1 "read" "not read") (#x8 "unsent (a draft)") (#x4 "being sent") (#x10 "with attachments")
    (#x20 "from the mailbox's own user") (#x40 "hidden, kept by a folder for itself") (#x80 "to be sent again")
    (#x2 "not changed since it was received"))
  "The message flags (PidTagMessageFlags) said in words, each (BIT WORDS
WORDS-WHEN-NOT-SET).")

(defun msg-flag-words (flags)
  "The message flags FLAGS, a number, in words."
  (and (integerp flags)
       (format nil "~{~a~^, ~}"
               (loop for (bit set unset) in *msg-flags*
                     for words = (if (logtest flags bit) set unset)
                     when words collect words))))

(defparameter *msg-item-limit* 1000
  "The most items one Outlook item is read as, itself and those attached to
it at every depth; those past it are listed as not read.")

(defvar *msg-items* nil
  "While an Outlook item is read, the storages read as items, a list.")

(defun msg-sender (mail properties codepage)
  "Set who MAIL is from by the item's PROPERTIES, text in code page CODEPAGE:
whom it was sent for, else who sent it; and note the item's kind, its flags
and the sender as the item gives it."
  (flet ((text (id) (msg-string properties id codepage))
         (value (id) (msg-value properties id)))
    (let ((sender (mail-person (text #x0C1A) (or (text #x5D01) (text #x0C1F)) (and (not (text #x5D01)) (text #x0C1E))))
          (for (mail-person (text #x0042) (or (text #x5D02) (text #x0065)) (and (not (text #x5D02)) (text #x0064)))))
      (setf (mail-from mail) (or for sender))
      (mail-detail mail "Item" (text #x001A))
      (mail-detail mail "Flags" (msg-flag-words (value #x0E07)))
      (when (and sender for (string/= sender for))
        (mail-detail mail "Sent by" (format nil "~a, for ~a" sender for)))
      (mail-detail mail "Sender as the item says" (mail-person (text #x0C1A) (text #x0C1F)))
      (mail-detail mail "Sender address type" (text #x0C1E)))))

(defun msg-recipients (mail cfb entry properties codepage)
  "Set whom MAIL is to, copied to and blind copied to, by the recipients'
storages in the item ENTRY of the compound file (CFB), text in code page
CODEPAGE; the item's own lists in PROPERTIES where it has no recipients of a
kind."
  (let ((to '()) (cc '()) (bcc '()))
    (dolist (recipient (msg-children-named cfb entry "__recip_version1.0_#"))
      (let* ((p (msg-properties cfb recipient 8))
             (person (mail-person (msg-string p #x3001 codepage)
                                  (or (msg-string p #x39FE codepage) (msg-string p #x3003 codepage))
                                  (and (not (msg-string p #x39FE codepage)) (msg-string p #x3002 codepage)))))
        (when person
          (case (msg-value p #x0C15)
            (2 (push person cc)) (3 (push person bcc)) (t (push person to))))))
    (flet ((joined (people display) (if people (format nil "~{~a~^, ~}" (reverse people)) display)))
      (setf (mail-to mail) (joined to (msg-string properties #x0E04 codepage))
            (mail-cc mail) (joined cc (msg-string properties #x0E03 codepage))
            (mail-bcc mail) (joined bcc (msg-string properties #x0E02 codepage))))))

(defun msg-body (mail properties codepage)
  "Set MAIL's body by the item's PROPERTIES, text in code page CODEPAGE: the
plain text, else the HTML, else the compressed Rich Text Format (RTF), each read
as text; its HTML, the item's own or the HTML its RTF keeps; and note which of
them the item holds."
  (let* ((plain (msg-string properties #x1000 codepage))
         (html (gethash #x1013 properties))
         (rtf (msg-value properties #x1009))
         (held (remove nil (list (and plain "plain text") (and html "HTML") (and rtf "RTF")))))
    (flet ((html-source ()
             (if (stringp (cdr html))
                 (cdr html)
                 (let* ((octets (cdr html))
                        (head (octets-latin-1 octets :end (min 2048 (length octets))))
                        (charset (multiple-value-bind (m g)
                                     (cl-ppcre:scan-to-strings "(?i)charset\\s*=\\s*[\"']?([-\\w:.]+)" head)
                                   (and m (aref g 0)))))
                   (charset-text octets (or charset (mail-codepage-charset (msg-value properties #x3FDE)) "utf-8"))))))
      (handler-case
          (cond (plain (setf (mail-body mail) plain (mail-body-kind mail) :plain))
                (html (setf (mail-body mail) (html-text (html-source)) (mail-body-kind mail) :html))
                ((vectorp rtf) (setf (mail-body mail) (rtf-text (lzfu-decompress rtf)) (mail-body-kind mail) :rtf)))
        (error (e) (mail-note mail "Its body could not be read: ~a" e)))
      (setf (mail-html mail) (cond (html #'html-source)
                                   ((vectorp rtf) (lambda () (nth-value 1 (rtf-text (lzfu-decompress rtf))))))))
    (mail-detail mail "Bodies it holds" (and held (format nil "~{~a~^, ~}" held)))))

(defun msg-attachments (mail cfb entry codepage depth)
  "Set MAIL's attachments by the attachments' storages in the item ENTRY of
compound file (CFB), text in code page CODEPAGE, the item being DEPTH deep: each
one's name, type and contents, an attached item read as one while DEPTH is below
*OUTLOOK-NESTING-LIMIT*; one that cannot be read is noted."
  (dolist (storage (msg-children-named cfb entry "__attach_version1.0_#"))
    (handler-case
        (let* ((p (msg-properties cfb storage 8))
               (data (gethash #x3701 p))
               (name (or (msg-string p #x3707 codepage) (msg-string p #x3704 codepage) (msg-string p #x3001 codepage)))
               (attachment (make-mail-attachment :name name :mime (msg-string p #x370E codepage)
                                                 :hidden (msg-value p #x7FFE)
                                                 :content-id (msg-string p #x3712 codepage))))
          (cond ((null data) (setf (mail-attachment-why attachment) "no contents"))
                ((vectorp (cdr data)) (setf (mail-attachment-octets attachment) (cdr data)))
                ((and (consp (cdr data)) (eql (msg-value p #x3705) 5))
                 ;; an item attached: read as one, to a depth, never one being read already
                 (if (and (< depth *outlook-nesting-limit*) (< (length *msg-items*) *msg-item-limit*)
                          (not (member (cdr (cdr data)) *msg-items*)))
                     (let ((inner (msg-mail cfb (cdr (cdr data)) :depth (1+ depth) :header 24)))
                       (setf (mail-attachment-message attachment) inner)
                       (unless name
                         (setf (mail-attachment-name attachment) (or (mail-subject inner) "an attached item"))))
                     (setf (mail-attachment-why attachment)
                           "an item nested too deep, or too often, to read (*outlook-nesting-limit*)")))
                (t (setf (mail-attachment-why attachment) "an embedded object, not read")))
          (unless (mail-attachment-name attachment)
            (setf (mail-attachment-name attachment) (format nil "attachment ~a" (subseq (cfb-entry-name storage) 20))))
          (push attachment (mail-attachments mail)))
      (error (e) (mail-note mail "An attachment could not be read: ~a" e))))
  (setf (mail-attachments mail) (nreverse (mail-attachments mail))))

(defun msg-mail (cfb entry &key (depth 0) (header 32))
  "The MAIL the item in the storage ENTRY of the compound file (CFB) is (the
root for an item of its own, HEADER 32; an attached one, HEADER 24). It has
its sender, recipients, dates and body (its plain text, else its HTML read as
text, else its compressed Rich Text Format (RTF) read as text), what it says
of itself, its internet headers, and its attachments. An attached item is
read as one when DEPTH is below *OUTLOOK-NESTING-LIMIT*."
  (push entry *msg-items*)
  (let* ((properties (msg-properties cfb entry header))
         (codepage (or (msg-value properties #x3FFD) (msg-value properties #x3FDE)))
         (mail (make-mail)))
    (flet ((text (id) (msg-string properties id codepage))
           (value (id) (msg-value properties id)))
      (msg-sender mail properties codepage)
      (msg-recipients mail cfb entry properties codepage)
      (setf (mail-subject mail) (text #x0037)
            (mail-sent mail) (value #x0039)
            (mail-received mail) (value #x0E06)
            (mail-message-id mail) (text #x1035)
            (mail-headers mail) (let ((h (text #x007D))) (and h (string-right-trim '(#\Return #\Newline #\Nul) h))))
      (mail-detail mail "Created" (time-words (value #x3007)))
      (mail-detail mail "Entry identifier" (let ((id (value #x0FFF))) (and (vectorp id) (octets-hex id))))
      (mail-detail mail "Last changed" (time-words (value #x3008)))
      (mail-detail mail "Last changed by" (text #x3FFA))
      (mail-detail mail "In reply to" (text #x1042))
      (mail-detail mail "Conversation" (text #x0070))
      (mail-detail mail "Importance" (case (value #x0017) (0 "low") (2 "high")))
      (mail-detail mail "Sensitivity" (case (value #x0036) (1 "personal") (2 "private") (3 "confidential")))
      (mail-detail mail
                   "Code page"
                   (and codepage (format nil "~d~@[ (~a)~]" codepage (mail-codepage-charset codepage))))
      (msg-body mail properties codepage)
      (msg-attachments mail cfb entry codepage depth)
      mail)))

(defparameter *msg-field-names*
  '((#x0017 "importance") (#x001A "message-class") (#x0026 "priority") (#x0036 "sensitivity") (#x0037 "subject")
    (#x0039 "client-submit-time") (#x003B "sent-representing-search-key") (#x003D "subject-prefix")
    (#x0042 "sent-representing-name") (#x0050 "reply-recipient-names") (#x0057 "message-to-me")
    (#x0058 "message-cc-me") (#x0064 "sent-representing-address-type") (#x0065 "sent-representing-email-address")
    (#x0070 "conversation-topic") (#x0071 "conversation-index") (#x0C1A "sender-name") (#x0C1D "sender-search-key")
    (#x0C1E "sender-address-type") (#x0C1F "sender-email-address") (#x0E02 "display-bcc") (#x0E03 "display-cc")
    (#x0E04 "display-to") (#x0E06 "message-delivery-time") (#x0E07 "message-flags") (#x0E08 "message-size")
    (#x0E1D "normalized-subject") (#x0E09 "parent-entry-id") (#x0FF9 "record-key") (#x0FFF "entry-id")
    (#x1035 "internet-message-id")
    (#x1039 "internet-references") (#x1042 "in-reply-to-id") (#x1046 "original-message-id") (#x3007 "creation-time")
    (#x3008 "last-modification-time") (#x300B "search-key") (#x3FDE "internet-codepage")
    (#x3FFA "last-modifier-name") (#x3FFD "message-codepage") (#x5D01 "sender-smtp-address")
    (#x5D02 "sent-representing-smtp-address"))
  "The names Microsoft's MS-OXPROPS gives the properties of an Outlook item,
each (NUMBER NAME), as the fields of a message are named (MSG-FIELDS).")

(defparameter *msg-property-sets*
  '(("00020328-0000-0000-C000-000000000046" "mapi") ("00020329-0000-0000-C000-000000000046" "public")
    ("00020386-0000-0000-C000-000000000046" "internet-headers") ("00062002-0000-0000-C000-000000000046" "appointment")
    ("00062003-0000-0000-C000-000000000046" "task") ("00062004-0000-0000-C000-000000000046" "address")
    ("00062008-0000-0000-C000-000000000046" "common") ("0006200A-0000-0000-C000-000000000046" "log")
    ("0006200B-0000-0000-C000-000000000046" "sticky-note") ("6ED8DA90-450B-101B-98DA-00AA003F1305" "meeting")
    ("41F28F13-83F4-4114-A584-EEDB5A6B0BFF" "messaging") ("00062040-0000-0000-C000-000000000046" "sharing"))
  "The property sets Microsoft's MS-OXPROPS names, each (GUID WORD), WORD what
the field of a named property of the set named by a number begins with
\(MSG-FIELD-NAME).")

(defun msg-field-name (id names)
  "The name of the field of the property numbered ID of an Outlook item whose
named properties NAMES says what they stand for (MSG-NAME-TABLE): that
MS-OXPROPS gives it (*MSG-FIELD-NAMES*); for a named one, its name, or its
property set's word and its number (address-8083), the set written out when
it has no word (*MSG-PROPERTY-SETS*); else 0x and its number."
  (let ((named (and (>= id #x8000) (gethash id names))))
    (cond ((assoc id *msg-field-names*) (second (assoc id *msg-field-names*)))
          ((and named (stringp (cdr named))) (cdr named))
          (named (format nil "~a-~(~4,'0x~)"
                         (or (second (find (car named) *msg-property-sets* :key (lambda (set) (pst-guid (first set)))
                                                                            :test #'equalp))
                             (octets-hex (car named)))
                         (cdr named)))
          (t (format nil "0x~(~4,'0x~)" id)))))

(defparameter *msg-contents-properties* '(#x007D #x1000 #x1009 #x1013)
  "The properties of an Outlook item that hold its contents, not what it says
of itself: its internet headers and its bodies, plain, compressed RTF and HTML.")

(defun msg-fields (octets)
  "What the Outlook item in OCTETS (a .msg) says of itself, each (NAME . TEXT):
each of its properties but those of its contents (*MSG-CONTENTS-PROPERTIES*),
NAME the name MS-OXPROPS gives it or what a named one stands for
\(MSG-FIELD-NAME);
TEXT a string as it is, a time in universal time, importance, sensitivity and
the message flags in words, a number written, octets in hexadecimal. In the
order of their numbers; an error when OCTETS are no Outlook item."
  (let* ((cfb (cfb-read octets))
         (properties (msg-properties cfb (aref (cfb-entries cfb) 0) 32))
         (names (msg-name-table cfb))
         (codepage (or (msg-value properties #x3FFD) (msg-value properties #x3FDE)))
         (fields '()))
    (maphash (lambda (id entry)
               (unless (member id *msg-contents-properties*)
                 (destructuring-bind (type . value) entry
                   (let ((text (cond ((eql id #x0017) (case value (0 "low") (1 "normal") (2 "high")))
                                     ((eql id #x0036) (case value (0 "none") (1 "personal") (2 "private")
                                                        (3 "confidential")))
                                     ((eql id #x0E07) (msg-flag-words value))
                                     ((eql type #x0040) (utc-time-words value))
                                     ((eql type #x000B) (if value "true" "false"))
                                     ((and (eql type #x0102) (vectorp value)) (octets-hex value))
                                     ((consp value) nil)
                                     (t (msg-string properties id codepage)))))
                     (when (and text (plusp (length text)))
                       (push (list id (msg-field-name id names) (one-line text)) fields))))))
             properties)
    (mapcar (lambda (field) (cons (second field) (third field))) (sort fields #'< :key #'first))))

(defun msg-read (octets)
  "The MAIL the Outlook item in OCTETS (a .msg) is; an error saying why when
it is not one, or is made to be read without end."
  (let ((cfb (cfb-read octets)) (*msg-items* '()))
    (msg-mail cfb (aref (cfb-entries cfb) 0))))

;;; An Outlook item as a message of a mailbox written

(defun msg-eight-bit-text (octets charset)
  "OCTETS, a string of an Outlook item, as text, the null after it left out:
in CHARSET when one is given, else in UTF-8 when they are UTF-8, else in
Windows-1252."
  (let ((end (or (position 0 octets) (length octets))))
    (if charset
        (charset-text octets charset :end end)
        (let ((text (octets-string octets :end end)))
          (if (find-if (lambda (c) (<= #xDC80 (char-code c) #xDCFF)) text)
              (charset-text octets "windows-1252" :end end)
              text)))))

(defun msg-name-table (cfb)
  "The named properties of the Outlook item in the compound file (CFB), from
the map in its storage __nameid_version1.0 ([MS-OXMSG] 2.2.3): a table from
each property number from #x8000 to (GUID . NAME) (PST-NAME-TABLE); empty
when it has none."
  (let* ((root (aref (cfb-entries cfb) 0))
         (map (find "__nameid_version1.0" (cfb-children cfb root) :key #'cfb-entry-name :test #'string=)))
    (if (and map (eq (cfb-entry-type map) :storage))
        (flet ((stream (name)
                 (let ((child (find name (cfb-children cfb map) :key #'cfb-entry-name :test #'string=)))
                   (and child (eq (cfb-entry-type child) :stream) (cfb-stream-octets cfb child)))))
          (pst-name-table (stream "__substg1.0_00020102") (stream "__substg1.0_00030102")
                          (stream "__substg1.0_00040102")))
        (make-hash-table))))

(defun msg-pst-properties (cfb entry header charset depth names)
  "The properties of the storage ENTRY of the compound file (CFB), HEADER
octets before its fixed-size ones (MSG-STORED-PROPERTIES), as PST-MESSAGE
keeps them: a named one kept when NAMES (MSG-NAME-TABLE) says what it stands
for, else left out, a string of eight-bit characters read in
CHARSET (MSG-EIGHT-BIT-TEXT) and made Unicode, a string's null left out, a
fixed-size value cut to its size, and an attached item read as a message
\(MSG-PST-MESSAGE) while DEPTH is below *OUTLOOK-NESTING-LIMIT*, never one
being read already, and no more of them than *MSG-ITEM-LIMIT*."
  (loop for (tag . raw) in (msg-stored-properties cfb entry header)
        for type = (pst-property-type tag)
        for value = (cond ((and (>= (ldb (byte 16 16) tag) #x8000) (not (gethash (ldb (byte 16 16) tag) names))) nil)
                          ((pst-fixed-size type) (subseq raw 0 (pst-fixed-size type)))
                          ((= type #x001E) (pst-text (msg-eight-bit-text raw charset)))
                          ((= type #x001F)
                           (let ((end (length raw)))
                             (loop while (and (>= end 2) (zerop (aref raw (- end 1))) (zerop (aref raw (- end 2))))
                                   do (decf end 2))
                             (subseq raw 0 end)))
                          ((= type #x000D)
                           (and (< depth *outlook-nesting-limit*) (< (length *msg-items*) *msg-item-limit*)
                                (not (member (cdr raw) *msg-items*))
                                (msg-pst-message cfb (cdr raw) :header 24 :depth (1+ depth) :charset charset
                                                                :names names)))
                          (t raw))
        when value collect (cons (if (= type #x001E) (logior (logandc2 tag #xFFFF) #x001F) tag) value)))

(defun msg-pst-message (cfb entry &key (header 32) (depth 0) charset (names (msg-name-table cfb)))
  "The item in the storage ENTRY of the compound file (CFB) (an item of its
own, HEADER 32; an attached one, 24) as a PST-MESSAGE, to be written in a
mailbox: its properties, its recipients' and its attachments', as
MSG-PST-PROPERTIES reads them, its strings of eight-bit characters in the
code page it names, else in CHARSET, and what its named properties stand
for, from NAMES, the item's map of them (MSG-NAME-TABLE); an attached item as
a message to a depth. A recipient with neither a name nor an address is left
out."
  (push entry *msg-items*)
  (let* ((properties (msg-properties cfb entry header))
         (charset (or (mail-codepage-charset (or (msg-value properties #x3FFD) (msg-value properties #x3FDE)))
                      charset)))
    (flet ((each (prefix)
             (loop for storage in (msg-children-named cfb entry prefix)
                   collect (msg-pst-properties cfb storage 8 charset depth names))))
      (let ((own (msg-pst-properties cfb entry header charset depth names))
            (recipients (remove-if-not (lambda (recipient)
                                         (some (lambda (tag) (plusp (length (pst-value recipient tag))))
                                               '(#x3001001F #x3003001F #x39FE001F)))
                                       (each "__recip_version1.0_#")))
            (attachments (each "__attach_version1.0_#")))
        (make-pst-message
         :properties own :recipients recipients :attachments attachments
         :names (loop for id being the hash-keys of names using (hash-value name)
                      when (some (lambda (properties) (find id properties :key (lambda (p) (ldb (byte 16 16) (car p)))))
                                 (cons own (append recipients attachments)))
                        collect (cons id name)))))))

;;; A message of a mailbox written as an Outlook item

(defun msg-multiple-values (type octets)
  "The values of a property of several values of TYPE (the type with #x1000
set), OCTETS as a mailbox keeps them ([MS-PST] 2.3.3.4), each an octet
vector: those of a fixed size cut in turn; those of any size by the count
and offsets they begin with. NIL when OCTETS are not as that says."
  (let ((size (pst-fixed-size (logand type #x0FFF))))
    (if size
        (loop for at from 0 to (- (length octets) size) by size collect (subseq octets at (+ at size)))
        (let ((count (and (>= (length octets) 4) (octets-integer octets 0 4))))
          (when (and count (<= (* 4 (1+ count)) (length octets)))
            (loop for i below count
                  for start = (octets-integer octets (* 4 (1+ i)) 4)
                  for end = (if (< (1+ i) count) (octets-integer octets (* 4 (+ 2 i)) 4) (length octets))
                  when (<= start end (length octets)) collect (subseq octets start end)))))))

(defun msg-stored-streams (properties header &key counts)
  "The streams and storages of an Outlook item's storage holding PROPERTIES,
each (TAG . VALUE) as PST-MESSAGE keeps them (a message attached as a
PST-MESSAGE, made a storage by MSG-ITEM-STORAGE), laid out as [MS-OXMSG]
2.4 says: the stream of properties, HEADER octets of it before them (32 for
an item of its own, 24 for one attached, 8 for a recipient or an
attachment), COUNTS its recipients' and attachments' numbers for the first
two; a stream for each value of any size, a string with its null after it;
a stream for each of several values. Children as CFB-WRITE takes them."
  (let ((fixed (pst-octets header)) (records '()) (children '()))
    (when counts
      (destructuring-bind (recipients attachments) counts
        (pst-put fixed 8 recipients 4) (pst-put fixed 12 attachments 4)
        (pst-put fixed 16 recipients 4) (pst-put fixed 20 attachments 4)))
    (loop for (tag . value) in properties
          for type = (pst-property-type tag)
          for size = (pst-fixed-size type)
          for name = (format nil "__substg1.0_~8,'0X" tag)
          do (let ((record (pst-octets 16)))
               (pst-put record 0 tag 4)
               (pst-put record 4 6 4)
               (cond ((and size (<= size 8)) (replace record value :start1 8 :end2 (min 8 (length value))))
                     ((pst-message-p value)
                      (pst-put record 8 #xFFFFFFFF 4)
                      (push (cons name (msg-item-storage value 24)) children))
                     ((logtest type #x1000)
                      (let ((values (msg-multiple-values type value)))
                        (if (pst-fixed-size (logand type #x0FFF))
                            (progn (pst-put record 8 (length value) 4) (push (cons name value) children))
                            (let* ((null (case type (#x101F 2) (#x101E 1) (t 0)))
                                   (lengths (pst-join (loop for v in values
                                                            collect (pst-number (+ (length v) null) 4)
                                                            when (= type #x1102) collect (pst-octets 4)))))
                              (pst-put record 8 (length lengths) 4)
                              (push (cons name lengths) children)
                              (loop for v in values for i from 0
                                    do (push (cons (format nil "~a-~8,'0X" name i)
                                                   (pst-join (list v (pst-octets null))))
                                             children))))))
                     (t (let ((stream (case type
                                        (#x001F (pst-join (list value (pst-octets 2))))
                                        (#x001E (pst-join (list value (pst-octets 1))))
                                        (t value))))
                          (pst-put record 8 (length stream) 4)
                          (push (cons name stream) children))))
               (push record records)))
    (cons (cons "__properties_version1.0" (pst-join (cons fixed (nreverse records)))) (nreverse children))))

(defun msg-item-storage (message header)
  "The children of the storage of the Outlook item MESSAGE, a PST-MESSAGE,
HEADER octets before its properties (32 of its own, 24 attached): its
properties (MSG-STORED-STREAMS), a storage for each recipient and each
attachment, numbered from 0, each with its number among its properties."
  (let ((recipients (pst-message-recipients message)) (attachments (pst-message-attachments message)))
    (append (msg-stored-streams (pst-message-properties message) header
                                :counts (list (length recipients) (length attachments)))
            (loop for recipient in recipients for i from 0
                  collect (cons (format nil "__recip_version1.0_#~8,'0X" i)
                                (msg-stored-streams (cons (pst-property #x30000003 i)
                                                          (remove #x30000003 recipient :key #'car))
                                                    8)))
            (loop for attachment in attachments for i from 0
                  collect (cons (format nil "__attach_version1.0_#~8,'0X" i)
                                (msg-stored-streams (cons (pst-property #x0E210003 i)
                                                          (remove #x0E210003 attachment :key #'car))
                                                    8))))))

(defun msg-name-streams (names)
  "The storage of named properties of an Outlook item ([MS-OXMSG] 2.2.3):
NAMES, a table from (GUID . NAME) to the number from #x8000 given it, as the
streams of property sets, of entries and of names a string, laid out as a
mailbox's map of named properties is (PST-NAME-STREAMS)."
  (multiple-value-bind (guids entries strings) (pst-name-streams names)
    (list (cons "__substg1.0_00020102" guids)
          (cons "__substg1.0_00030102" entries)
          (cons "__substg1.0_00040102" strings))))

(defun msg-write (message &key entry-id)
  "MESSAGE, a PST-MESSAGE, as an Outlook item (.msg, [MS-OXMSG]), octets: a
compound file (CFB-WRITE) of its properties, recipients, attachments and
messages attached, and its named properties' map; ENTRY-ID, when given, the
entry identifier it had in its mailbox, among its properties."
  (let* ((names (make-hash-table :test 'equalp))
         (message (pst-renamed message names))
         (properties (pst-message-properties message)))
    (when entry-id
      (setf properties (cons (cons #x0FFF0102 entry-id) (remove #x0FFF0102 properties :key #'car))))
    (setf (pst-message-properties message) properties)
    (cfb-write (append (msg-item-storage message 32)
                       (list (cons "__nameid_version1.0" (msg-name-streams names)))))))
