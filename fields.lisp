;;;; fields.lisp --- The fields of a mailbox's messages: their headers and what Outlook says of them, filtered alone.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; As each message of a mailbox is read for the list, its fields are written
;;;; beside the text a search looks through: every header, each on a line of
;;;; its own as often as the message has it (Received many times), the names
;;;; of its attachments, and what the Outlook item written of it says of
;;;; itself, each property by the name Microsoft gives it (outlook-importance,
;;;; outlook-conversation-topic), a named property by what it stands for. The names found in the mailbox are kept with
;;;; the list, and Tuatara's filters of a list's parts (C-x / f, C-x / h) offer
;;;; them: a filter on one tries its pattern on that field's values alone, and
;;;; a message without the field is no match. Nothing in a field is run.

(in-package #:outlook)

(defparameter *mailbox-first-fields* '("from" "to" "cc" "bcc" "subject" "date" "attachment name" "your label")
  "The fields of a mailbox's messages offered first, in this order, when a
filter asks for one; the others found in the mailbox follow by name. Those
Tuatara makes up have a space in their names, which no header's name may
hold, so that a header of a message can never pose as one of them.")

(defun mailbox-header-name-p (name)
  "True when NAME may be the name of a header: printable characters but the
colon, no space (RFC 5322). Only such headers are fields, so that none can
take the name of a field Tuatara makes up (*MAILBOX-FIRST-FIELDS*)."
  (and (plusp (length name)) (every (lambda (c) (and (char< #\Space c #\Rubout) (char/= c #\:))) name)))

(defun mailbox-field-words (name)
  "What a filter's question says of the field NAME beside it: a header, the
name of an attachment, a label of yours, or what the Outlook item says."
  (cond ((string= name "attachment name") "the name of an attachment")
        ((string= name "your label") "a label you put on")
        ((uiop:string-prefix-p "outlook " name) "what the Outlook item says of itself")
        (t "a header")))

(defun mailbox-message-fields (part msg)
  "The fields of the message PART (MIME-READ), each (NAME . VALUE), NAME in
lower case: each of its headers whose name may be one (MAILBOX-HEADER-NAME-P),
as often as it has it, encoded words decoded, on one line; attachment name
and the name of each attachment; and what the Outlook item at MSG, the same
message written as an Outlook item, says of itself, outlook and a space before each
property's name (MSG-FIELDS), when MSG is given and can be read."
  (append (loop for (name . value) in (mime-part-headers part)
                when (mailbox-header-name-p name)
                  collect (cons name (one-line (mime-decode-words value))))
          (let ((leaves (mime-leaves part)))
            (let ((body (mime-body-part leaves)))
              (loop for leaf in leaves
                    for name = (mime-part-name leaf)
                    when (and name (mime-attachment-p leaf body))
                      collect (cons "attachment name" (one-line name)))))
          (and msg
               (handler-case
                   (call-with-file msg (lambda (fd file size)
                                         (and (<= size *outlook-largest-file*)
                                              (loop with octets = (read-fd-octets fd :path file :size-hint (1+ size))
                                                    for (name . value) in (msg-fields octets)
                                                    collect (cons (format nil "outlook ~a" name) value)))))
                 (error () nil)))))

(defun mailbox-fields-text (fields)
  "The text of the file of a message's FIELDS (MAILBOX-MESSAGE-FIELDS): each
on a line, its name, a colon and a space, then its value."
  (with-output-to-string (out)
    (loop for (name . value) in fields do (format out "~a: ~a~%" name value))))

(defun mailbox-field-values (text field)
  "The values FIELD (a name in lower case) has in TEXT, the file of a
message's fields (MAILBOX-FIELDS-TEXT), each on a line; NIL when it has none."
  (let ((prefix (format nil "~a: " field)) (values '()))
    (dolist (line (uiop:split-string text :separator (string #\Newline)))
      (when (uiop:string-prefix-p prefix line)
        (push (subseq line (length prefix)) values)))
    (and values (format nil "~{~a~^~%~}" (nreverse values)))))

(defun mailbox-item-field-text (item field)
  "The values the field FIELD of ITEM, a message of a mailbox's list, has,
each on a line, as written for it (MAILBOX-ITEM-OF); NIL when it has none,
or when they cannot be read."
  (let ((file (and (mailbox-item-p item) (mailbox-item-fields-file item))))
    (and file
         (let ((text (handler-case
                         (call-with-file file (lambda (fd file size)
                                                (octets-string (read-fd-octets fd :path file :size-hint (1+ size)))))
                       (file-system-error () nil))))
           (and text (mailbox-field-values text field))))))

(defun mailbox-field-names (found)
  "The fields a filter offers, each (NAME . WORDS) (MAILBOX-FIELD-WORDS), FOUND
the table of those found in the mailbox: those of *MAILBOX-FIRST-FIELDS* first,
then the rest by name."
  (let ((rest '()))
    (maphash (lambda (name value)
               (declare (ignore value))
               (unless (member name *mailbox-first-fields* :test #'string=) (push name rest)))
             found)
    (mapcar (lambda (name) (cons name (mailbox-field-words name)))
            (append *mailbox-first-fields* (sort rest #'string<)))))

(defun mailbox-light-field (field regex)
  "The regular expression lighting in a message opened the matches of REGEX,
a filter on FIELD: from the field's name to the match on a header's line, so
that which header matched shows; the match itself for an attachment's name;
nothing for a label or what the Outlook item says of itself, which the
message does not show as a line of its own."
  (cond ((string= field "attachment name") regex)
        ((or (string= field "your label") (uiop:string-prefix-p "outlook " field)) "(?!)")
        (t (format nil "(?m:^(?i:~a):[^\\n]*?(?:~a))" (pattern-regex field nil) regex))))
