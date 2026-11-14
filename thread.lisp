;;;; thread.lisp --- The earlier messages of a conversation found in the same mailbox, for their attachments.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A reply or a forward quotes the earlier mails of its conversation, but
;;;; seldom carries their attachments. When the mailbox holds those earlier
;;;; mails, a message opened from its list shows their attachments at its end,
;;;; each opened as the message's own are. The earlier mails are found by the
;;;; Message-IDs the message names in its In-Reply-To and References headers,
;;;; and theirs in turn; else by Outlook's conversation index (the Thread-Index
;;;; header, or the item's own PR_CONVERSATION_INDEX): its first 22 octets
;;;; name the conversation and each reply adds 5, so a message whose index is
;;;; the start of this one's is earlier in the same conversation. The links
;;;; of each message are read as it is listed and looked up in tables made
;;;; once, after the listing, in the background.

(in-package #:outlook)

(defparameter *mailbox-thread-most* 1000
  "The most earlier messages followed from one message by their Message-IDs,
however the mailbox links them, so that a loop of replies ends.")

(defun mailbox-message-ids (text)
  "The Message-IDs TEXT names, <...> each, in order; NIL for none."
  (and text (loop for (start end) on (cl-ppcre:all-matches "<[^<>\\s]+>" text) by #'cddr
                  collect (subseq text start end))))

(defun mailbox-thread-of (part fields)
  "How the message PART (MIME-READ), with FIELDS (MAILBOX-MESSAGE-FIELDS),
links to the earlier messages of its conversation, as (ID EARLIER INDEX
SOURCE): ID its Message-ID; EARLIER the Message-IDs it names, each (ID .
HEADER), HEADER In-Reply-To or References; INDEX its conversation index in
hexadecimal and SOURCE where it was read, :HEADER for Thread-Index, :ITEM for
the Outlook item; each NIL when it has none."
  (let* ((header (let ((text (mime-header part "thread-index")))
                   (and text (handler-case (octets-hex (base64-octets (string-octets (string-trim " " text))))
                               (error () nil)))))
         (item (and (null header) (cdr (assoc "outlook conversation-index" fields :test #'string=))))
         (index (or header item)))
    (list (first (mailbox-message-ids (mime-header part "message-id")))
          (remove-duplicates (append (mapcar (lambda (id) (cons id "In-Reply-To"))
                                             (mailbox-message-ids (mime-header part "in-reply-to")))
                                     (mapcar (lambda (id) (cons id "References"))
                                             (mailbox-message-ids (mime-header part "references"))))
                             :test #'string= :key #'car :from-end t)
          (and index (>= (length index) 44) (string-downcase index))
          (and index (>= (length index) 44) (if header :header :item)))))

(defun mailbox-thread-tables (folders)
  "The tables finding the messages of FOLDERS, each (NAME . ITEMS), by their
links (MAILBOX-ITEM-THREAD): one from each Message-ID, one from each
conversation index, to the items that have it. O(messages)."
  (let ((by-id (make-hash-table :test 'equal)) (by-index (make-hash-table :test 'equal)))
    (loop for (nil . items) in folders
          do (dolist (item items)
               (destructuring-bind (&optional id earlier index source) (mailbox-item-thread item)
                 (declare (ignore earlier source))
                 (when id (push item (gethash id by-id)))
                 (when index (push item (gethash index by-index))))))
    (cons by-id by-index)))

(defun mailbox-earlier-items (tables item)
  "The messages earlier in ITEM's conversation that carry attachments, found
by TABLES (MAILBOX-THREAD-TABLES), each (MESSAGE . HOW), HOW words saying how
it was found: those its In-Reply-To and References name, and those theirs name
in turn, as many as *MAILBOX-THREAD-MOST*; else those whose conversation index
is the start of ITEM's. A Message-ID that several messages give is said. In
the order they were sent, ITEM never among them."
  (destructuring-bind (&optional id earlier index source) (mailbox-item-thread item)
    (declare (ignore id))
    (let ((seen (make-hash-table :test 'eq)) (found '()))
      (setf (gethash item seen) t)
      (loop with waiting = (mapcar (lambda (link) (list (car link) (cdr link) nil)) earlier)
            while (and waiting (< (hash-table-count seen) *mailbox-thread-most*))
            do (destructuring-bind (named header by) (pop waiting)
                 (let ((others (gethash named (car tables))))
                   (dolist (other others)
                     (unless (gethash other seen)
                       (setf (gethash other seen) t)
                       (push (cons other (format nil "by ~a~@[ of ~a~]~:[~*~;; ~a is the Message-ID of ~:d messages~]"
                                                 header by (rest others) named (length others)))
                             found)
                       (dolist (link (second (mailbox-item-thread other)))
                         (setf waiting (append waiting (list (list (car link) (cdr link) (mailbox-item-name other)))))))))))
      (when (and (null found) index)
        (loop for end from 44 below (length index) by 10
              do (dolist (other (gethash (subseq index 0 end) (cdr tables)))
                   (unless (gethash other seen)
                     (setf (gethash other seen) t)
                     (push (cons other (format nil "by conversation index: ~:[Outlook item~;header~]" (eq source :header)))
                           found)))))
      (sort (remove-if-not (lambda (other) (plusp (mailbox-item-attachments (car other)))) found)
            #'< :key (lambda (other) (or (mailbox-item-sent (car other)) 0))))))

(defun mailbox-earlier-words (item how)
  "What the list of earlier attachments says of ITEM, the message they came
with, found HOW: its subject, whom from, when it was sent and how it was found."
  (format nil "~a~@[, from ~a~]~@[, ~a~] (~a)" (mailbox-item-name item) (mailbox-item-from item)
          (short-time-words (mailbox-item-sent item)) how))
