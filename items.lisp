;;;; items.lisp --- The items of a mailbox read written as files: mail (.eml), contacts (.vcf), appointments (.ics).
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each item read from a mailbox (pst-read.lisp), a PST-MESSAGE, is written
;;;; as the files the list of the mailbox shows and opens (mailbox.lisp). A
;;;; message, and any item but a contact, an appointment, a task or a journal
;;;; entry, is written as internet mail: its internet headers as it came when
;;;; it holds them, else headers made of its properties; its plain text and
;;;; its HTML (or the HTML or text its compressed RTF keeps); and its
;;;; attachments, a message attached written as mail within it. Beside it the
;;;; same item as an Outlook item (.msg), every property kept. A contact is
;;;; written as a vCard, an appointment, a task or a journal entry as
;;;; iCalendar. What was noted as it was read (a check that did not match, a
;;;; part that could not be read) is written into its mail as headers of
;;;; their own, X-Tuatara-Reading-Note, so that it shows where the message is
;;;; read.

(in-package #:outlook)

;;; Properties as values

(defun item-charset (properties)
  "The charset the eight-bit strings among PROPERTIES are in: that of the
item's code page, else of its internet code page, else windows-1252."
  (or (some (lambda (tag) (let ((value (pst-value properties tag)))
                            (and value (mail-codepage-charset (octets-integer value 0 4)))))
            '(#x3FFD0003 #x3FDE0003))
      "windows-1252"))

(defun item-text (properties id &optional (charset (item-charset properties)))
  "The string property numbered ID among PROPERTIES as text (PST-STRING-TEXT),
of Unicode or of eight-bit characters in CHARSET; NIL for none or an empty
one."
  (let* ((tag (find-if (lambda (tag) (assoc tag properties))
                       (list (logior (ash id 16) #x001F) (logior (ash id 16) #x001E))))
         (text (and tag (pst-string-text tag (pst-value properties tag) charset))))
    (and text (plusp (length text)) text)))

(defun item-integer (properties tag)
  "The whole number the property TAG among PROPERTIES holds, or NIL."
  (let ((value (pst-value properties tag)))
    (and value (octets-integer value 0 (length value)))))

(defun item-time (properties tag)
  "The universal time the time property TAG among PROPERTIES holds, or NIL."
  (filetime-universal-time (item-integer properties tag)))

(defun item-named (message guid name)
  "The value of MESSAGE's named property NAME (a number or a string) of the
property set GUID (PST-GUID), (TAG . VALUE), or NIL."
  (let ((guid (pst-guid guid)))
    (loop for (id set . own) in (pst-message-names message)
          when (and (equalp set guid) (equal own name))
            return (find id (pst-message-properties message)
                         :key (lambda (property) (ldb (byte 16 16) (car property)))))))

(defun item-named-text (message guid name)
  "The text of MESSAGE's named string property NAME of the set GUID, or NIL."
  (let ((property (item-named message guid name)))
    (and property (member (pst-property-type (car property)) '(#x001F #x001E))
         (let ((text (pst-string-text (car property) (cdr property) (item-charset (pst-message-properties message)))))
           (and (plusp (length text)) text)))))

(defun item-named-time (message guid name)
  "The universal time MESSAGE's named time property NAME of the set GUID
holds, or NIL."
  (let ((property (item-named message guid name)))
    (and property (= #x0040 (pst-property-type (car property)))
         (filetime-universal-time (octets-integer (cdr property) 0 8)))))

(defun item-class (properties)
  "The message class of an item's PROPERTIES (IPM.Note, IPM.Contact), or
IPM.Note when it gives none."
  (or (item-text properties #x001A) "IPM.Note"))

(defun item-kind (message)
  "How MESSAGE is written: :contact as a vCard, :event, :task or :journal as
iCalendar, else :mail."
  (let ((class (string-downcase (item-class (pst-message-properties message)))))
    (flet ((is (prefix) (or (string= class prefix) (uiop:string-prefix-p (concatenate 'string prefix ".") class))))
      (cond ((is "ipm.contact") :contact)
            ((is "ipm.appointment") :event)
            ((is "ipm.task") :task)
            ((is "ipm.activity") :journal)
            (t :mail)))))

;;; Internet mail

(defun item-date-words (time)
  "The universal time TIME as mail writes a date (RFC 5322), in universal
time: Tue, 06 Oct 2026 09:00:00 +0000."
  (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time time 0)
    (format nil "~a, ~2,'0d ~a ~d ~2,'0d:~2,'0d:~2,'0d +0000"
            (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday) day
            (aref *short-month-names* (1- month)) year hour minute second)))

(defun item-address (name address)
  "A person as a header names them: NAME in encoded words when it is not
ASCII, quoted, then ADDRESS in angle brackets; either alone when the other
is missing or the same; NIL for neither."
  (let ((name (and name (plusp (length (string-trim " " name))) (one-line (string-trim " " name))))
        (address (and address (plusp (length (string-trim " " address))) (one-line (string-trim " " address)))))
    (cond ((and name address (string-not-equal name address))
           (format nil "~a <~a>" (if (mime-ascii-p name)
                                     (format nil "\"~a\"" (remove #\" name))
                                     (mime-encode-words name))
                   address))
          (address (format nil "<~a>" address))
          (name (mime-encode-words name)))))

(defun item-sender (properties)
  "Who sent the item of PROPERTIES, as a header names them: whom it was sent
for, else who sent it, by their SMTP addresses where it holds them."
  (or (item-address (item-text properties #x0042) (or (item-text properties #x5D02) (item-text properties #x0065)))
      (item-address (item-text properties #x0C1A) (or (item-text properties #x5D01) (item-text properties #x0C1F)))))

(defun item-recipient-lines (message)
  "The headers To, Cc and Bcc of MESSAGE, each (NAME . VALUE), made of its
recipients of each kind, else of the item's own lists of names."
  (let ((properties (pst-message-properties message)))
    (loop for (kind header own) in '((1 "To" #x0E04) (2 "Cc" #x0E03) (3 "Bcc" #x0E02))
          for people = (loop for recipient in (pst-message-recipients message)
                             when (eql kind (or (item-integer recipient #x0C150003) 1))
                               collect (item-address (item-text recipient #x3001 (item-charset properties))
                                                     (or (item-text recipient #x39FE) (item-text recipient #x3003))))
          for value = (if (remove nil people)
                          (format nil "~{~a~^, ~}" (remove nil people))
                          (let ((names (item-text properties own))) (and names (mime-encode-words names))))
          when value collect (cons header value))))

(defun item-transport-lines (text)
  "The internet headers TEXT, as an item keeps them, as lines of mail: lines
ended by line feeds, the empty ones and those neither a header nor its
continuation left out, as are the headers of its MIME structure
\(Content-Type, Content-Transfer-Encoding, MIME-Version), which the mail
written has of its own."
  (let ((keep t))
    (with-output-to-string (out)
      (dolist (line (uiop:split-string (remove #\Return text) :separator '(#\Newline)))
        (cond ((zerop (length line)))
              ((member (char line 0) '(#\Space #\Tab))
               (when keep (write-line line out)))
              ((let ((colon (position #\: line)))
                 (and colon (plusp colon) (mailbox-header-name-p (subseq line 0 colon))))
               (setf keep (not (member (string-downcase (subseq line 0 (position #\: line)))
                                       '("content-type" "content-transfer-encoding" "mime-version")
                                       :test #'string=)))
               (when keep (write-line line out)))
              (t (setf keep nil)))))))

(defun item-header-text (message notes)
  "The headers of MESSAGE as mail, but those of its MIME structure: its
internet headers when it holds them (ITEM-TRANSPORT-LINES), else headers
made of its properties (date, sender, recipients, subject, Message-ID and
those it replies to); then NOTES, each an X-Tuatara-Reading-Note."
  (let* ((properties (pst-message-properties message))
         (transport (item-text properties #x007D))
         (lines (and transport (item-transport-lines transport))))
    (with-output-to-string (out)
      (if (and lines (plusp (length lines)))
          (write-string lines out)
          (let ((time (or (item-time properties #x00390040) (item-time properties #x0E060040)
                          (item-time properties #x30070040))))
            (flet ((put (name value) (when value (write-string (mime-header-line name value) out))))
              (put "Date" (and time (item-date-words time)))
              (put "From" (item-sender properties))
              (loop for (name . value) in (item-recipient-lines message) do (put name value))
              (put "Subject" (let ((subject (item-text properties #x0037))) (and subject (mime-encode-words subject))))
              (put "Message-ID" (item-text properties #x1035))
              (put "In-Reply-To" (item-text properties #x1042))
              (put "References" (item-text properties #x1039)))))
      (dolist (note notes)
        (write-string (mime-header-line "X-Tuatara-Reading-Note" (mime-encode-words note)) out)))))

(defun item-mail-part (headers body)
  "A part of mail as octets: HEADERS, a list of header lines (MIME-HEADER-LINE),
an empty line, then BODY, a string (written in UTF-8) or octets."
  (pst-join (list (string-octets (format nil "~{~a~}~%" headers))
                  (if (stringp body) (string-octets body) body))))

(defun item-may-hold-p (part marker)
  "True when PART, the octets of a part of mail made here (ITEM-MAIL-PART),
may hold MARKER, octets beginning =_: its headers looked through, where a
part of parts names its own boundary, and the whole of it when it is a
message; the base64 and quoted-printable of any other cannot hold = followed
by _."
  (let ((end (or (search #(10 10) part) (length part))))
    (or (search marker part :end2 (min (length part) (+ end 2)))
        (and (search (load-time-value (string-octets "message/rfc822")) part :end2 end)
             (search marker part :start2 end)))))

(defun item-multipart (subtype parts)
  "PARTS, each the octets of a part of mail (ITEM-MAIL-PART), as one
multipart part of SUBTYPE (mixed, alternative): its Content-Type header line
and its body, two values. Its boundary is one none of PARTS holds
\(ITEM-MAY-HOLD-P)."
  (let ((boundary (loop for n from 0
                        for boundary = (format nil "=_tuatara_~d" n)
                        for marker = (string-octets boundary)
                        unless (some (lambda (part) (item-may-hold-p part marker)) parts) return boundary)))
    (values (mime-header-line "Content-Type" (format nil "multipart/~a; boundary=\"~a\"" subtype boundary))
            (pst-join (append (loop for part in parts
                                    collect (string-octets (format nil "--~a~%" boundary))
                                    collect part
                                    collect (string-octets (string #\Newline)))
                              (list (string-octets (format nil "--~a--~%" boundary))))))))

(defun item-text-part (text)
  "TEXT as a part of plain text in UTF-8, quoted-printable."
  (item-mail-part (list (mime-header-line "Content-Type" "text/plain; charset=\"utf-8\"")
                        (mime-header-line "Content-Transfer-Encoding" "quoted-printable"))
                  (mime-quoted-printable text)))

(defun item-html-part (html charset)
  "HTML, a string or octets in CHARSET (NIL when they name their own), as a
part of HTML, quoted-printable."
  (item-mail-part (list (mime-header-line "Content-Type"
                                          (format nil "text/html~@[; charset=\"~a\"~]"
                                                  (if (stringp html) "utf-8" charset)))
                        (mime-header-line "Content-Transfer-Encoding" "quoted-printable"))
                  (mime-quoted-printable html)))

(defun item-bodies (properties notes)
  "The parts of the text of the item of PROPERTIES: its plain text and its
HTML, each when it holds it, else what its compressed RTF holds of them; a
list of parts of mail, plain text first. What cannot be read is pushed on
the car of NOTES."
  (let* ((charset (item-charset properties))
         (plain (item-text properties #x1000 charset))
         (html (let ((binary (pst-value properties #x10130102)))
                 (or (and binary (plusp (length binary)) binary) (item-text properties #x1013 charset))))
         (rtf (pst-value properties #x10090102)))
    (when (and rtf (not html))
      (handler-case (multiple-value-bind (text kept) (rtf-text (lzfu-decompress rtf))
                      (setf html kept)
                      (unless plain (setf plain text)))
        (error (condition) (push (format nil "its compressed RTF could not be read: ~a" condition) (car notes)))))
    (remove nil (list (and plain (item-text-part plain))
                      (and html (item-html-part html
                                                (and (not (stringp html))
                                                     (mail-codepage-charset (item-integer properties #x3FDE0003)))))))))

(defun item-attachment-name (attachment index charset)
  "The name of ATTACHMENT, the INDEXth (from 1) of its message: its long
file name, else its file name, else its display name, else attachment and
its number."
  (or (item-text attachment #x3707 charset) (item-text attachment #x3704 charset)
      (item-text attachment #x3001 charset) (format nil "attachment ~d" index)))

(defun item-attachment-part (attachment index charset depth)
  "ATTACHMENT, the properties of the INDEXth attachment of a message DEPTH
deep, as a part of mail: a message attached as mail within (ITEM-EML); a
file in base64 with its name, its type and the identifier its message's
HTML shows it by; one whose contents the mailbox does not hold as a short
text saying so, named for it."
  (let* ((name (item-attachment-name attachment index charset))
         (inner (pst-value attachment #x3701000D))
         (data (pst-value attachment #x37010102))
         (type (let ((type (item-text attachment #x370E charset)))
                 (if (and type (cl-ppcre:scan "^[A-Za-z0-9!#$&^_.+-]+/[A-Za-z0-9!#$&^_.+-]+$" type))
                     (string-downcase type)
                     "application/octet-stream")))
         (content-id (item-text attachment #x3712 charset)))
    (cond ((pst-message-p inner)
           (item-mail-part (list (mime-header-line "Content-Type" "message/rfc822")
                                 (mime-header-line "Content-Disposition"
                                                   (format nil "attachment; ~a" (mime-parameter "filename" name))))
                           (item-eml inner :depth (1+ depth))))
          (data
           (item-mail-part (remove nil (list (mime-header-line "Content-Type"
                                                               (format nil "~a; ~a" type (mime-parameter "name" name)))
                                             (mime-header-line "Content-Disposition"
                                                               (format nil "~:[attachment~;inline~]; ~a"
                                                                       content-id (mime-parameter "filename" name)))
                                             (and content-id
                                                  (mime-header-line "Content-ID"
                                                                    (format nil "<~a>" (string-trim "<> " content-id))))
                                             (mime-header-line "Content-Transfer-Encoding" "base64")))
                           (mime-base64 data)))
          (t (let ((method (item-integer attachment #x37050003)))
               (item-mail-part (list (mime-header-line "Content-Type" "text/plain; charset=\"utf-8\"")
                                     (mime-header-line "Content-Disposition"
                                                       (format nil "attachment; ~a"
                                                               (mime-parameter "filename"
                                                                               (format nil "~a (not in the mailbox).txt"
                                                                                       name))))
                                     (mime-header-line "Content-Transfer-Encoding" "quoted-printable"))
                               (mime-quoted-printable
                                (format nil "The mailbox does not hold the contents of the attachment ~a~a.~%"
                                        name (case method
                                               ((2 3 4) ", which is a link to a file kept elsewhere")
                                               (t ", or they could not be read"))))))))))

(defun item-eml (message &key (depth 0) notes)
  "MESSAGE, a PST-MESSAGE DEPTH deep, as internet mail, octets: its headers
\(ITEM-HEADER-TEXT, NOTES among them), then its text and its attachments, as
one part when it has but one, else in parts (multipart/mixed, the plain
text and HTML of its text as multipart/alternative)."
  (let* ((properties (pst-message-properties message))
         (charset (item-charset properties))
         (said (list '()))
         (bodies (item-bodies properties said))
         (attachments (loop for attachment in (pst-message-attachments message) for index from 1
                            collect (item-attachment-part attachment index charset depth)))
         (headers (item-header-text message (append notes (reverse (car said))))))
    (multiple-value-bind (content-type body)
        (let ((text (cond ((rest bodies) (multiple-value-call #'item-pair-part (item-multipart "alternative" bodies)))
                          (bodies (first bodies)))))
          (cond (attachments (item-multipart "mixed" (if text (cons text attachments) attachments)))
                (text (item-split-part text))
                (t (item-split-part (item-text-part "")))))
      (pst-join (list (string-octets headers)
                      (string-octets (mime-header-line "MIME-Version" "1.0"))
                      (string-octets content-type)
                      body)))))

(defun item-pair-part (content-type body)
  "A multipart part of mail as octets, CONTENT-TYPE its header line and BODY
its octets (ITEM-MULTIPART)."
  (item-mail-part (list content-type) body))

(defun item-split-part (part)
  "PART, the octets of a part of mail (ITEM-MAIL-PART), as two values: the
text of its headers, and the octets from the empty line ending them on, so
that they follow a message's own headers."
  (let ((end (search (string-octets (format nil "~%~%")) part)))
    (values (octets-string part :end (1+ end)) (subseq part (1+ end)))))

;;; Contacts and appointments

(defun item-escaped (text)
  "TEXT as a value of a vCard or of iCalendar holds it: backslashes, commas
and semicolons escaped, each line break written \\n."
  (with-output-to-string (out)
    (loop for c across (remove #\Return text)
          do (case c
               (#\Newline (write-string "\\n" out))
               ((#\\ #\, #\;) (write-char #\\ out) (write-char c out))
               (t (write-char c out))))))

(defun item-folded (lines)
  "LINES, each a content line of a vCard or of iCalendar, folded as RFC 5545
says, no line longer than 75 octets, each ended by a carriage return and a
line feed, joined."
  (with-output-to-string (out)
    (dolist (line lines)
      (let ((size 0))
        (loop for c across line
              for octets = (string-octet-length (string c))
              do (when (> (+ size octets) 75)
                   (format out "~c~c " #\Return #\Newline)
                   (setf size 1))
                 (write-char c out)
                 (incf size octets)))
      (format out "~c~c" #\Return #\Newline))))

(defun item-vcard (message)
  "MESSAGE, a contact, as a vCard (RFC 2426, version 3.0): its names, its
email addresses, telephone numbers, company, title and notes."
  (let* ((properties (pst-message-properties message))
         (charset (item-charset properties))
         (address-set "00062004-0000-0000-C000-000000000046"))
    (flet ((text (id) (item-text properties id charset)))
      (item-folded
       (remove nil
               (append
                (list "BEGIN:VCARD" "VERSION:3.0"
                      (format nil "FN:~a" (item-escaped (or (text #x3001) (text #x0037) ""))))
                (list (format nil "N:~{~a~^;~}"
                              (mapcar (lambda (id) (item-escaped (or (text id) "")))
                                      '(#x3A11 #x3A06 #x3A44 #x3A45 #x3A05))))
                (loop for lid in '(#x8083 #x8093 #x80A3)
                      for address = (item-named-text message address-set lid)
                      when address collect (format nil "EMAIL;TYPE=INTERNET:~a" (item-escaped address)))
                (loop for (id kind) in '((#x3A08 "WORK") (#x3A09 "HOME") (#x3A1C "CELL") (#x3A24 "WORK,FAX")
                                         (#x3A25 "HOME,FAX"))
                      when (text id) collect (format nil "TEL;TYPE=~a:~a" kind (item-escaped (text id))))
                (list (and (text #x3A16) (format nil "ORG:~a" (item-escaped (text #x3A16))))
                      (and (text #x3A17) (format nil "TITLE:~a" (item-escaped (text #x3A17))))
                      (and (text #x1000) (format nil "NOTE:~a" (item-escaped (text #x1000))))
                      "END:VCARD")))))))

(defun item-ical-time (time)
  "The universal time TIME as iCalendar writes one in universal time:
20261006T090000Z."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time time 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour minute second)))

(defun item-icalendar (message nid)
  "MESSAGE, an appointment, a task or a journal entry, the node NID of its
mailbox, as iCalendar (RFC 5545): an event, a to-do or a journal entry with
its subject, times, place, organiser and attendees, and notes."
  (let* ((properties (pst-message-properties message))
         (charset (item-charset properties))
         (kind (item-kind message))
         (component (case kind (:event "VEVENT") (:task "VTODO") (t "VJOURNAL")))
         (appointment "00062002-0000-0000-C000-000000000046")
         (task "00062003-0000-0000-C000-000000000046"))
    (flet ((text (id) (item-text properties id charset))
           (when-time (name time) (and time (format nil "~a:~a" name (item-ical-time time)))))
      (item-folded
       (remove nil
               (append
                (list "BEGIN:VCALENDAR" "VERSION:2.0" "PRODID:-//Tuatara//outlook package//EN"
                      (format nil "BEGIN:~a" component)
                      (format nil "UID:~(~x~)@mailbox" nid)
                      (when-time "DTSTAMP" (or (item-time properties #x30080040) (item-time properties #x30070040)
                                               (get-universal-time)))
                      (when-time "CREATED" (item-time properties #x30070040))
                      (when-time "LAST-MODIFIED" (item-time properties #x30080040))
                      (and (text #x0037) (format nil "SUMMARY:~a" (item-escaped (text #x0037)))))
                (case kind
                  (:event (list (when-time "DTSTART" (or (item-named-time message appointment #x820D)
                                                         (item-time properties #x00600040)))
                                (when-time "DTEND" (or (item-named-time message appointment #x820E)
                                                       (item-time properties #x00610040)))
                                (let ((place (item-named-text message appointment #x8208)))
                                  (and place (format nil "LOCATION:~a" (item-escaped place))))))
                  (:task (list (when-time "DTSTART" (item-named-time message task #x8104))
                               (when-time "DUE" (item-named-time message task #x8105))))
                  (t (list (when-time "DTSTART" (item-time properties #x00600040)))))
                (let ((organiser (item-text properties #x0042)))
                  (list (and organiser (format nil "ORGANIZER;CN=~a:mailto:~a" (item-escaped organiser)
                                               (item-escaped (or (item-text properties #x0065) organiser))))))
                (loop for recipient in (pst-message-recipients message)
                      for name = (item-text recipient #x3001 charset)
                      for address = (or (item-text recipient #x39FE charset) (item-text recipient #x3003 charset))
                      when (or name address)
                        collect (format nil "ATTENDEE~@[;CN=~a~]:mailto:~a"
                                        (and name (item-escaped name)) (item-escaped (or address name))))
                (list (and (text #x1000) (format nil "DESCRIPTION:~a" (item-escaped (text #x1000))))
                      (format nil "END:~a" component) "END:VCALENDAR")))))))

;;; The files of an item

(defun item-files (message nid number notes &key record-key)
  "The files MESSAGE, the node NID of its mailbox, is written as, numbered
NUMBER in its folder, each (NAME . OCTETS): a contact as NUMBER.vcf, an
appointment, a task or a journal entry as NUMBER.ics, anything else as
NUMBER.eml (ITEM-EML, NOTES among its headers) and NUMBER.msg (MSG-WRITE),
its entry identifier made of its mailbox's RECORD-KEY, when given, and NID."
  (case (item-kind message)
    (:contact (list (cons (format nil "~d.vcf" number) (string-octets (item-vcard message)))))
    ((:event :task :journal) (list (cons (format nil "~d.ics" number) (string-octets (item-icalendar message nid)))))
    (t (list (cons (format nil "~d.eml" number) (item-eml message :notes notes))
             (cons (format nil "~d.msg" number)
                   (msg-write message :entry-id (and record-key (= 16 (length record-key))
                                                     (pst-entry-id record-key nid))))))))
