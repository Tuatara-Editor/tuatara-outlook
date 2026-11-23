;;;; mailbox.lisp --- Outlook mailboxes (.pst, .ost) read and listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mailbox is read by the package's own reader (pst-read.lisp), never in
;;;; the editor: in a Tuatara of its own shut in by the sandbox (SANDBOX-CALL),
;;;; which reads the mailbox where it is, read only, its access time left as
;;;; it was, and writes in one directory alone; the editor reads only what it
;;;; wrote. The mailbox is checked to be as it was summed just before and
;;;; after (for a mailbox on another machine, the copy Tuatara read from
;;;; there). Each item is written as files (items.lisp) into a new directory
;;;; for you alone in the cache directory, a directory for each folder of the
;;;; mailbox: a message as internet mail (.eml) and as an Outlook item (.msg),
;;;; every property kept, so that those marked can be copied out as either,
;;;; collected into one mbox file, or written into a new mailbox (pst.lisp); a
;;;; message that cannot be read as a file saying why, so that none is left
;;;; out. It runs in the background, however large the mailbox, held to a
;;;; time, to a space on the drive and to the drive's free space, and to the
;;;; memory the sandbox gives it; the folders and messages written
;;;; are then listed, a line each, and opened as messages (view.lisp). As each
;;;; message is read for the list, the text a search of the list looks
;;;; through is written beside it, once: its headers, its text parts decoded
;;;; and its attachments' names, nothing else of an attachment decoded.
;;;; Tuatara's search filters narrow the list by it, matched in the
;;;; background. The mailbox itself is never changed. What was written is an
;;;; entry of the cache, found again by the mailbox's SHA-256 sum and the
;;;; options it was read with: the same mailbox opened again, here or from
;;;; another machine, is listed from it without reading it again, and it is
;;;; cleared as the cache clears what is unused (*CACHE-KEEP*, M-x cache-manager).

(in-package #:outlook)

(define-major-mode outlook-mailbox-mode sectioned-list-mode
  "An Outlook mailbox (personal storage table, .pst, or offline storage table,
.ost) read, with its folders, and under each
its messages, a line each: when it was sent, whom from, how many attachments
it has and its subject.
\\<outlook-mailbox-mode>\\[list-visit] or \\`l' opens the message on the line, read
only, the matches of the list's filters lit in it. \\`P' there shows it as it
looks and \\`q' comes back. \\[list-visit-other] opens it in another window,
the list staying. \\[list-next-entry] and \\[list-previous-entry] go to the
next item and the one before. On a folder's line it folds or unfolds the
folder, as \\[list-toggle-fold] does, and \\[list-toggle-all-folds] folds or
unfolds them all.
\\[list-mark-entry] marks the message on the line (>), \\[list-unmark-entry]
takes its mark off, \\[list-unmark-all] takes every mark off, and
\\[list-toggle-marks] marks those not marked and unmarks those marked.
\\[outlook-export] copies those marked, or the one on the line, out into a
directory, as saved mail (.eml) or as Outlook items (.msg), from a menu.
\[outlook-label] puts labels of your own on them, shown on their lines and
kept for the mailbox between sessions, and \[outlook-unlabel] takes them off;
\[outlook-filter-labels] narrows the list to the messages carrying a label.
The message on the line is shown beside the list as the cursor moves, read as
any message is (*OUTLOOK-PREVIEW*); \\[list-toggle-preview] turns that off and
on.
\\[quit-window] closes the list, \\[outlook-back-to-directory] too, after asking,
going back to its directory in rimu; the messages written of it stay in the
cache, to list the mailbox again without reading it, until unused for
*CACHE-KEEP* (\\[cache-manager]).
\\[regex-search-forward] and \\[regex-search-backward] narrow the list as a regular
expression is typed to the messages whose headers, any of them, or contents
match: the text of each text part, decoded, and the names of the
attachments; \\<search>\\[search-exit] keeps the pattern as a filter, the next search
narrowing what it left, and \\[search-abort] lets the one typed go.
\\<outlook-mailbox-mode>\\[search-filter-show-records] narrows the list by one field of
its messages, asked for first: a header (to, received, x-mailer, any the
mailbox has), an attachment's name, a label, or what the Outlook item says of
itself (outlook importance); \\[search-filter-hide-records] hides those
matched. \\[search-filter-list] lists the filters, to take one away, and
\\[search-filter-remove-all] takes them all away. The messages are searched in the
background, the mode line saying how far it has got; of a message larger
than *OUTLOOK-SEARCH-TEXT-LIMIT* characters only that much is searched, and
the list says how many there are."
  :lighter "Mailbox" :interactive nil)

(defvar *mailboxes* '()
  "The buffers listing a mailbox.")

(define-record (mailbox-item (:copier nil))
  "An item of a mailbox as written when it was read. PATH is its file. MSG is
the same message written as an Outlook item, or NIL. MESSAGE is true for a
message (else a contact, an appointment, written as is). SENT, FROM, SUBJECT
and ATTACHMENTS are as written to be read. TEXT-FILE is the file of what a
search of the list looks through in it, WHOLE NIL when that is only a part of
it. IDENTITY, FIELDS-FILE and THREAD say what it is known by, where its fields
are and how it links to its conversation."
  (path nil :documentation "The file of MAILBOX-ITEM.")
  (msg nil :documentation "The same message as an Outlook item, written as a file, or NIL.")
  (message nil
           :documentation "True when MAILBOX-ITEM is a message, else a contact or an appointment, written as it is.")
  (sent nil :documentation "When MAILBOX-ITEM was sent, a universal time, or NIL.")
  (from nil :documentation "Who MAILBOX-ITEM is from, as written to be read.")
  (subject nil :documentation "The subject of MAILBOX-ITEM, as written to be read.")
  (attachments 0 :documentation "How many attachments MAILBOX-ITEM has.")
  (text-file nil :documentation "The file of what a search of the list looks through in MAILBOX-ITEM, or NIL.")
  (whole t :documentation "NIL when the text file holds only a part of MAILBOX-ITEM, the message being too large.")
  (identity nil
            :documentation "What MAILBOX-ITEM is known by from one reading of the mailbox to the next
\(MAILBOX-MESSAGE-IDENTITY), or NIL.")
  (fields-file nil :documentation "The file of the fields of MAILBOX-ITEM (MAILBOX-MESSAGE-FIELDS), or NIL.")
  (thread nil
          :documentation "How MAILBOX-ITEM links to the earlier messages of its conversation (MAILBOX-THREAD-OF), or
NIL."))

(defun mailbox-preview-words (name size)
  "What the preview says of the mailbox NAME, of SIZE octets."
  (format nil "~a is an Outlook mailbox, ~a.~%~%~
               Opened (RET in a list), it is read and its folders and messages listed~
               ~:[~;, deleted items among them~].~%"
          name (octets-words size) *outlook-include-deleted*))

;;; What was written of a mailbox

(defun mailbox-files (root function &key limit)
  "Call FUNCTION on what was written of a mailbox under ROOT, 64 directories deep: with
the directory's path from ROOT (\"\" for ROOT) and NIL as each directory is
entered, and with that path, the name and the size of each regular file in
it. Links, whatever they lead to, and other entries are passed over. It stops
once LIMIT entries were seen, files and directories counted, and when its job
is asked to stop (JOB-CHECK-STOP); true when it stopped short of the end."
  (let ((seen 0))
    (not (walk-directory root
                         (lambda (directory relative entry)
                           (declare (ignore directory))
                           (incf seen)
                           (when (or (null entry) (zerop (mod seen 256)))
                             (job-check-stop))
                           (when (and limit (> seen limit))
                             (return-from mailbox-files t))
                           (if (null entry)
                               (funcall function relative nil nil)
                               (destructuring-bind (name kind size own &rest rest) entry
                                 (declare (ignore rest))
                                 (when (eq own kind)
                                   (case kind
                                     (:directory t)
                                     (:regular (funcall function relative name size) nil))))))
                         :max-depth 64))))

(defun mailbox-folders (root &key (limit 1000000) (in-order t))
  "The folders under ROOT, a directory a mailbox was written in, and ROOT itself,
each (RELATIVE . FILES): RELATIVE its path from ROOT (\"\" for ROOT), FILES
each (NAME . SIZE), its regular files, in the order of their numbers unless
IN-ORDER is NIL. Links and other entries are passed over; at most LIMIT
files, 64 folders deep, no more than twice LIMIT entries and 1,024 more
looked at, and true as a second value when there were more."
  (let ((found '()) (folders (make-hash-table :test 'equal)) (count 0) (more nil))
    (when (mailbox-files root
                         (lambda (relative name size)
                           (cond ((null name) (push (setf (gethash relative folders) (list relative)) found))
                                 ((< count limit)
                                  (incf count)
                                  (push (cons name size) (cdr (gethash relative folders))))
                                 (t (setf more t))))
                         :limit (+ (* 2 limit) 1024))
      (setf more t))
    (values
     (loop for folder in (nreverse found)
           when (cdr folder)
             collect (cons (car folder)
                           (if in-order
                               (sort (cdr folder)
                                     (lambda (a b)
                                       (let ((x (parse-integer a :junk-allowed t))
                                             (y (parse-integer b :junk-allowed t)))
                                         (if (and x y (/= x y)) (< x y) (string< a b))))
                                     :key #'car)
                               (cdr folder))))
     more)))

(defun mailbox-search-text (part)
  "What a search of a mailbox's list looks through in the message PART
\(MIME-READ), as two values: the text, and NIL when it was cut short at
*OUTLOOK-SEARCH-TEXT-LIMIT* characters, else true. The text is its headers,
every one on a line of its own, encoded words decoded; the name of each
attachment; the text of each text part, decoded from its transfer encoding and
charset, HTML as its text; and so on for each message attached, as far as they
were read. Nothing else of an attachment is decoded."
  (let ((out (make-string-output-stream)) (left *outlook-search-text-limit*) (whole t))
    (labels ((put (text)
               (let ((taken (min (length text) left)))
                 (write-string text out :end taken)
                 (write-char #\Newline out)
                 (decf left taken)
                 (when (< taken (length text)) (setf whole nil))))
             (walk (part)
               (loop for (name . value) in (mime-part-headers part)
                     while (plusp left)
                     do (put (format nil "~a: ~a" name (one-line (mime-decode-words value)))))
               (dolist (leaf (mime-leaves part))
                 (when (plusp left)
                   (let ((name (mime-part-name leaf)) (type (mime-part-type leaf)))
                     (when name (put name))
                     (cond ((mime-part-message leaf) (walk (mime-part-message leaf)))
                           ((uiop:string-prefix-p "text/" type)
                            ;; written larger than what is left to look through: read as far as that goes
                            (let ((most (* 4 left)))
                              (when (> (- (mime-part-body-end leaf) (mime-part-body-start leaf)) most) (setf whole nil))
                              (put (if (string= type "text/html")
                                       (html-text (mime-part-text leaf :most most))
                                       (mime-part-text leaf :most most)))))))))))
      (walk part))
    (values (get-output-stream-string out) (and whole (plusp left)))))

(defun mailbox-item-name (item)
  "What ITEM of a mailbox is called in questions and messages: its subject."
  (or (mailbox-item-subject item) "(no subject)"))

(defun mailbox-text-file (path text)
  "PATH, where TEXT for a search of a mailbox's list is kept: written there, a
new file for you alone, unless an earlier listing of the same messages wrote
it already, which is then read as it is."
  (flet ((written-p () (eq :regular (path-kind path :follow nil))))
    (if (written-p)
        path
        (handler-case (write-new-file path text)
          ;; another listing of the same messages wrote it meanwhile
          (file-system-error (condition)
            (if (written-p) path (error condition)))))))

(defun mailbox-item-of (path name &key text-path (folder "") msg field-names)
  "The MAILBOX-ITEM the file NAME at PATH, written in FOLDER as its mailbox
was read, is: a message (a name with no ending, or .eml) read for its date,
sender, subject and attachments counted, and known by its identity
\(MAILBOX-MESSAGE-IDENTITY); one that could not be read (.unreadable) by
why; any other as it is named. A message larger than *OUTLOOK-LARGEST-FILE*
is read for its headers alone. MSG is the same message as an Outlook item,
or NIL. With TEXT-PATH, a message's text for a search of the list
\(MAILBOX-SEARCH-TEXT) is written there, and its fields
\(MAILBOX-MESSAGE-FIELDS) beside it, TEXT-PATH.fields, new files for you
alone unless written already (MAILBOX-TEXT-FILE), the fields' names put in the
table FIELD-NAMES."
  (let ((type (native-file-type name)))
    (cond
      ((string-equal type "unreadable")
       (let ((why (or (ignore-errors (file-text path)) "")))
         (make-mailbox-item :path path :subject (format nil "(could not be read) ~a"
                                                        (one-line (string-trim '(#\Newline) why))))))
      ((not (or (null type) (string-equal type "eml")))
       (make-mailbox-item :path path :subject name))
      (t
       (handler-case
           (let* ((large nil)
                  (octets (call-with-file
                           path (lambda (fd file size)
                                  (if (> size *outlook-largest-file*)
                                      (progn (setf large t) (read-fd-start fd file size 65536))
                                      (read-fd-octets fd :path file :size-hint (1+ size))))))
                  (part (mime-read octets))
                  (leaves (mime-leaves part))
                  (body (mime-body-part leaves))
                  (item (flet ((header (name)
                                       (let ((value (mime-header part name)))
                                         (and value (one-line (mime-decode-words value))))))
                          (make-mailbox-item :path path :message t :sent (mail-date-time (mime-header part "date"))
                                             :from (header "from") :subject (header "subject")
                                             :attachments (count-if (lambda (leaf) (mime-attachment-p leaf body))
                                                                    leaves)
                                             :whole (not large) :identity (mailbox-message-identity part folder)))))
             (when text-path
               (multiple-value-bind (text whole) (mailbox-search-text part)
                 (setf (mailbox-item-text-file item) (mailbox-text-file text-path text)
                       (mailbox-item-whole item) (and whole (not large))))
               (let ((fields (mailbox-message-fields part msg)))
                 (when field-names
                   (loop for (field) in fields do (setf (gethash field field-names) t)))
                 (setf (mailbox-item-fields-file item)
                       (mailbox-text-file (concatenate 'string text-path ".fields") (mailbox-fields-text fields))
                       (mailbox-item-thread item) (mailbox-thread-of part fields))))
             item)
         (error () (make-mailbox-item :path path :message t :subject (format nil "~a (could not be read)" name))))))))

(defun mailbox-item-search-text (item)
  "What a search of a mailbox's list looks through in ITEM, an entry of the
list: a message's headers and contents as written for it (MAILBOX-ITEM-OF);
NIL for anything else, or when that cannot be read."
  (let ((file (and (mailbox-item-p item) (mailbox-item-text-file item))))
    (and file
         (ignore-errors
          (call-with-file file (lambda (fd file size)
                                 (octets-string (read-fd-octets fd :path file :size-hint (1+ size)))))))))

;;; Reading it

(define-record (mailbox-job (:include job)
                        (:constructor make-mailbox-job (buffer path directory &optional text-directory))
                        (:copier nil))
  "The mailbox at PATH, of SIZE octets, being read for BUFFER, a job: first
summed; then ITEMS-READ of its READING messages read and written into
DIRECTORY; then LISTED of TOTAL files read for the list. FOLDERS is what it
lists once done, each (NAME . ITEMS), and NOTES what to say of how it went.
SUM is the mailbox's SHA-256 sum, and LABELS the labels on its messages, kept
in the journal LABELS-NAME."
  (path nil :documentation "The mailbox MAILBOX-JOB reads.")
  (directory nil :documentation "The directory the messages of the mailbox are written into.")
  (text-directory nil
                  :documentation "The directory the text a search of the list looks through in each message is written
into, or NIL for none.")
  (stamp nil :documentation "The stamp of the mailbox as it was summed.")
  (seen nil
        :documentation "The stamp of the mailbox as its visit judged it when it came from outside (FILE-VIEW-STAMP), or
NIL.")
  (size 0 :documentation "The octets in the mailbox.")
  (reading nil :documentation "How many messages the mailbox holds, once known.")
  (items-read 0 :documentation "How many messages have been read and written so far.")
  (files 0 :documentation "How many files have been written so far.")
  (written nil
           :documentation "How many files the reader of the mailbox said it wrote, or NIL when it was not read this
time.")
  (octets 0 :documentation "How many octets have been written so far.")
  (listed 0 :documentation "How many files have been read for the list so far.")
  (total nil :documentation "How many files there are to read for the list, or NIL before it is known.")
  (folders nil :documentation "What MAILBOX-JOB lists once done, each (NAME . ITEMS).")
  (notes '() :documentation "What to say of how MAILBOX-JOB went, in words.")
  (summed nil
          :documentation "A list holding how many octets of the mailbox are summed so far, while they are, else NIL.")
  (sum nil :documentation "The SHA-256 sum of the mailbox, or NIL when it could not be made.")
  (labels nil
          :documentation "The labels on the messages of the mailbox, a table from each identity to its labels
\(MAILBOX-READ-LABELS), or NIL without a sum.")
  (labels-name nil
               :documentation "The name of the labels journal of the mailbox (MAILBOX-LABELS-NAME), taken as it is
listed.")
  (labels-writer nil :documentation "The job keeping the labels, the last one started.")
  (labels-pending nil
                  :documentation "The journal lines of the changes made since the labels writer took them
\(MAILBOX-KEEP-LABELS).")
  (field-names (make-hash-table :test 'equal)
               :documentation "A table of the names of the fields of the messages (MAILBOX-MESSAGE-FIELDS), found as
they are listed.")
  (threads nil
           :documentation "The tables finding the messages by their links to earlier ones (MAILBOX-THREAD-TABLES), made
once they are."))

(define-job-words mailbox-job
  :doing (cond ((mailbox-job-total job) "Listing the mailbox")
               ((mailbox-job-summed job) "Summing the mailbox")
               (t "Reading the mailbox"))
  :description (format nil "Reading the mailbox ~a" (native-file-name (mailbox-job-path job)))
  :verb "read the mailbox"
  :left-words "nothing listed")

(defmethod job-percent ((job mailbox-job))
  "How far summing, reading or listing the mailbox has got."
  (let ((total (mailbox-job-total job)))
    (cond (total (running-percent (mailbox-job-listed job) total))
          ((mailbox-job-reading job) (running-percent (mailbox-job-items-read job) (max 1 (mailbox-job-reading job))))
          ((mailbox-job-summed job)
           (running-percent (car (mailbox-job-summed job)) (max 1 (mailbox-job-size job)))))))

(defmethod job-state-words ((job mailbox-job))
  "How far reading the mailbox has got: the per cent summed, the items
written, then the per cent listed."
  (if (or (mailbox-job-total job) (mailbox-job-summed job))
      (call-next-method)
      (format nil "Reading the mailbox: ~:d of ~:d items, ~a written"
              (mailbox-job-items-read job) (or (mailbox-job-reading job) 0) (octets-words (mailbox-job-octets job)))))

(define-cache-kind "mailbox" "mailboxes/" "Mailboxes read"
  :open (lambda (directory key facts)
          (declare (ignore directory key))
          (let ((origin (mailbox-listed-origin facts)))
            (if origin (visit-path origin) (message "Where the mailbox was is not known")))))

(defun mailbox-cache-key (sum)
  "What finds the messages written of the mailbox whose SHA-256 sum is SUM in
the cache (FIND-CACHE-ENTRY): the sum, the options it is read with
\(*OUTLOOK-INCLUDE-DELETED*) and the word reader, so that an entry written
by readpst, which read mailboxes before, is not taken for it."
  (format nil "~a~:[~; deleted~] reader" sum *outlook-include-deleted*))

(defun mailbox-listed-origin (facts)
  "Where the mailbox was read from, a path or /ssh:HOST:/path, when FACTS,
noted of an entry of the cache, say all of it was written; else NIL."
  (and (uiop:string-prefix-p "listed " facts) (subseq facts 7)))

(defun mailbox-write-item (file directory relative number nid record-key)
  "Read the message NID of the mailbox FILE (a PST-FILE) and write it into
DIRECTORY at RELATIVE, its folder's path from there, as the files ITEM-FILES
makes of it, numbered NUMBER, its entry identifier made of the mailbox's
RECORD-KEY. A message that cannot be read or written, for whatever reason,
memory or the drive running out among them, is written as NUMBER.unreadable,
saying why, beside what was written of it. Returns how many files and
octets were written."
  (let ((made 0) (written 0))
    (flet ((put (name octets)
             (write-new-file (path-join directory (concatenate 'string relative name)) octets)
             (incf made)
             (incf written (length octets))))
      (handler-case
          (let* ((notes (list '()))
                 (message (let ((*pst-notes* notes)) (pst-read-message file nid))))
            (loop for (name . octets) in (item-files message nid number (reverse (car notes)) :record-key record-key)
                  do (put name octets)))
        (serious-condition (condition)
          (handler-case
              (put (format nil "~d.unreadable" number)
                   (string-octets (format nil "Message ~x of the mailbox could not be ~:[written~;read~]: ~a~%"
                                          nid (typep condition 'pst-unreadable)
                                          (handler-case (princ-to-string condition)
                                            (serious-condition () "an error that cannot be told")))))
            (serious-condition () nil)))))
    (values made written)))

(defun mailbox-write-folder (directory relative names)
  "Make the directory of a folder written of a mailbox, RELATIVE from
DIRECTORY, with *MAILBOX-PATH-FILE* holding NAMES, its path whole, when they
are given. How many files were written."
  (ensure-directories (path-join directory relative))
  (if names
      (progn (write-new-file (path-join directory (concatenate 'string relative *mailbox-path-file*))
                             (string-octets (format nil "~{~a~^/~}" names)))
             1)
      0))

(defun mailbox-reader-memory ()
  "The octets of memory the mailbox's reader shut in may take: 1 gigabyte
\(GB) and three times *OUTLOOK-LARGEST-FILE*."
  (+ (expt 2 30) (* 3 *outlook-largest-file*)))

(defun mailbox-message-budget (memory)
  "The most octets of blocks a message is read with by the reader shut in
with MEMORY octets (*PST-MESSAGE-BUDGET*): an eighth of the heap it has
\(SANDBOX-CALL-HEAP), and never more than *OUTLOOK-LARGEST-FILE*; so that a
message stops at its budget, keeping what was read of it, before writing it
out runs memory out."
  (min *outlook-largest-file* (floor (sandbox-call-heap memory) 8)))

(defun drive-full-watch (directory)
  "A function of how many files have been written into DIRECTORY, true when the
drive holding it has less than 1 gigabyte (GB) free: it looks at the drive the
first time and again each time 64 files or more have been written since."
  (let ((checked nil))
    (lambda (files)
      (when (or (null checked) (>= files (+ checked 64)))
        (setf checked files)
        (let ((free (ignore-errors (free-octets directory))))
          (and free (< free (expt 2 30))))))))

(defun mailbox-read-shut-in (inputs directory &key deleted seconds space largest-file nesting budget)
  "Read the mailbox, the first of INPUTS, and write each item of each folder
holding any (PST-LISTED-FOLDERS, deleted items as DELETED says) into
DIRECTORY, a directory for each folder (MAILBOX-WRITE-FOLDER,
MAILBOX-WRITE-ITEM), what was read reported as it goes, a list of the items
read, those to read, and the files and octets written. Run shut in by
MAILBOX-READ-PST, never in the editor, with the settings it is given:
LARGEST-FILE for *OUTLOOK-LARGEST-FILE*, NESTING for *OUTLOOK-NESTING-LIMIT*,
BUDGET for *PST-MESSAGE-BUDGET*.
It stops after SECONDS, once it has written more than SPACE octets, or when
the drive has less than 1 gigabyte (GB) free. A folder whose directory cannot
be made is passed over, its messages with it, and said among the problems.
Returns a list of how it went, \"done\", \"timeout\" or \"full\"; what was
found wrong in the mailbox, a list of words; and how many files it wrote. Or
\"unreadable\" and why, when the mailbox cannot be read at all, or \"failed\"
and why, when reading it failed part way; with the files written by then."
  (let ((*outlook-largest-file* largest-file)
        (*outlook-nesting-limit* nesting)
        (*pst-message-budget* budget)
        (deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))
        (read 0) (files 0) (octets 0) (problems '())
        (drive-full (drive-full-watch directory)))
    (flet ((words (condition)
             (handler-case (princ-to-string condition)
               (serious-condition () "an error that cannot be told"))))
      (handler-case
          (with-pst-file (file (first inputs))
            (multiple-value-bind (folders cut) (pst-listed-folders file :deleted deleted)
              (let ((record-key (nth-value 1 (pst-store file)))
                    (total (reduce #'+ folders :key (lambda (folder) (length (cdr folder))))))
                (flet ((ended (how)
                         (return-from mailbox-read-shut-in
                           (list how (append (reverse (pst-file-problems file)) (reverse problems)) files))))
                  (loop for (names . nids) in folders
                        for relative = (format nil "~{~a/~}" names)
                        for made = (handler-case
                                       (progn (incf files (mailbox-write-folder directory relative
                                                                                (cdr (assoc names cut :test #'equal))))
                                              t)
                                     (serious-condition (condition)
                                       (push (format nil "folder ~a could not be written, nor its ~:d messages: ~a"
                                                     relative (length nids) (words condition))
                                             problems)
                                       (incf read (length nids))
                                       nil))
                        when made
                          do (loop for nid in nids for number from 1
                                   do (when (> (get-internal-real-time) deadline) (ended "timeout"))
                                      (when (or (> octets space) (funcall drive-full files))
                                        (ended "full"))
                                      (multiple-value-bind (made written)
                                          (mailbox-write-item file directory relative number nid record-key)
                                        (incf read)
                                        (incf files made)
                                        (incf octets written))
                                      (sandbox-call-report (list read total files octets))))
                  (ended "done")))))
        (pst-unreadable (condition) (list "unreadable" (words condition) files))
        (serious-condition (condition) (list "failed" (words condition) files))))))

(defun mailbox-read-pst (job fd)
  "Have the mailbox open as FD, JOB's, read and each of its items
written into JOB's directory by the package's reader shut in
\(MAILBOX-READ-SHUT-IN, through SANDBOX-CALL): it reads that very file, read
only, whatever is put at its path meanwhile, and writes in JOB's directory alone;
the editor never reads the mailbox itself, only the files written. It stops
when JOB is asked to, after *OUTLOOK-MAILBOX-SECONDS*, once it has written
more than *OUTLOOK-MAILBOX-SPACE*, or when the drive has less than 1 gigabyte
\(GB) free, what it wrote by then listed. Returns four values: how it went,
:DONE, :TIMEOUT, :FULL, or :STOPPED when the reader failed, ran out of memory
or wrote more than may be looked through; what was found wrong in the
mailbox, a list of words; how many files the reader said it wrote, or NIL;
and for :STOPPED, why, in words. A mailbox that cannot be read at all, one
that changed while it was read, or no sandbox to read it in, is an error
saying why."
  (multiple-value-bind (result failure outcome)
      (sandbox-call 'mailbox-read-shut-in (list fd) (mailbox-job-directory job)
                    :arguments (list :deleted *outlook-include-deleted* :seconds *outlook-mailbox-seconds*
                                     :space *outlook-mailbox-space* :largest-file *outlook-largest-file*
                                     :nesting *outlook-nesting-limit*
                                     :budget (mailbox-message-budget (mailbox-reader-memory)))
                    :seconds (+ *outlook-mailbox-seconds* 60)
                    :memory (mailbox-reader-memory)
                    :file-limit (* 2 *outlook-largest-file*)
                    :label "Reading the mailbox"
                    :progress (lambda (counts)
                                (when (and (vectorp counts) (= 4 (length counts)) (every #'integerp counts))
                                  (setf (mailbox-job-items-read job) (aref counts 0)
                                        (mailbox-job-reading job) (aref counts 1)
                                        (mailbox-job-files job) (aref counts 2)
                                        (mailbox-job-octets job) (aref counts 3)))))
    (job-check-stop)
    (let* ((shaped (and (vectorp result) (= 3 (length result)) (stringp (aref result 0))
                        (integerp (aref result 2))))
           (how (and shaped (aref result 0)))
           ;; no problems at all is an empty list, which JSON gives back as null
           (said (and shaped (if (eq (aref result 1) :null) #() (aref result 1))))
           (files (and shaped (aref result 2))))
      (cond ((member outcome '(:timeout :full)) (values outcome '() nil nil))
            ((member outcome '(:failed :memory :too-much)) (values :stopped '() nil failure))
            ((not (eq outcome :done)) (error "~a" failure))
            ((equal how "unreadable") (error "The mailbox could not be read: ~a" said))
            ((and (equal how "failed") (stringp said))
             (values :stopped '() files (format nil "Reading the mailbox failed: ~a" said)))
            ((and (member how '("done" "timeout" "full") :test #'equal) (vectorp said) (every #'stringp said))
             (values (intern (string-upcase how) :keyword) (coerce said 'list) files nil))
            (t (error "The mailbox's reader said what it did in words it does not use"))))))

(defun mailbox-sum (job fd path)
  "Sum the mailbox open as FD, PATH's, JOB's, with SHA-256, as its labels are
kept by (MAILBOX-READ-LABELS) and what was written of it is found again in
the cache (MAILBOX-CACHE-KEY), its access time left as it was; NIL when it
cannot be read, and stopped when JOB is asked to stop."
  (setf (mailbox-job-summed job) (list 0))
  (unwind-protect (fd-sha256 fd path :cancel (job-cancel job) :done (mailbox-job-summed job))
    (setf (mailbox-job-summed job) nil)
    (job-check-stop)))

(defun mailbox-take-entry (job entry)
  "Have JOB list from ENTRY, an entry of the cache: the mailbox's items in its
mail/, the text searched in each in its text/. Both are written once, by the
listing that made the entry, before it is noted as whole; a later listing, in
this session or another, only reads them (MAILBOX-ITEM-OF), so that no listing
changes what another is reading."
  (let ((messages (path-join entry "mail/"))
        (texts (path-join entry "text/")))
    (ensure-directories messages)
    (ensure-directories texts)
    (setf (mailbox-job-directory job) messages
          (mailbox-job-text-directory job) texts)))

(defun mailbox-hold-copy (job origin)
  "Have the list of JOB's mailbox, a copy Tuatara read from ORIGIN on another
machine, use the copy's entry of the cache while it is open, so that it is not
cleared while it is read or the list shows it; refused when the copy is
gone or is no longer as read (REMOTE-COPY-ORIGIN)."
  (unless (and (find-cache-entry "remote" origin :user (job-buffer job))
               (equal origin (remote-copy-origin (mailbox-job-path job))))
    (mailbox-copy-gone origin)))

(defun mailbox-copy-gone (origin)
  "Refuse to go on with a mailbox whose copy from ORIGIN, on another machine,
is gone or no longer as read."
  (error "The copy of the mailbox read from ~a is gone or changed" origin))

(defun mailbox-read-afresh (job origin fd)
  "Have JOB's mailbox, open as FD, read and its items written into JOB's
directory (MAILBOX-READ-PST), what was found wrong in it noted, and say how
it went: :DONE when all was read. The file read is the one summed, checked
just before and after to be as it was then: by its stamp, and a mailbox from
another machine (ORIGIN, its name there, the mailbox the copy Tuatara read)
by its copy too (REMOTE-COPY-ORIGIN). Where the reader stopped short, what
it wrote by then is listed and the notes say why."
  (let ((path (mailbox-job-path job)))
    (flet ((check (words)
             (when (and origin (not (equal origin (remote-copy-origin path))))
               (if words (error "The copy of the mailbox changed while it was read") (mailbox-copy-gone origin)))
             (unless (equalp (mailbox-job-stamp job) (fd-stamp fd path))
               (error "The mailbox changed ~:[since it was summed~;while it was read~]" words))))
      (multiple-value-bind (outcome problems files why)
          (progn (check nil)
                 (multiple-value-prog1 (mailbox-read-pst job fd)
                   (check t)))
        (setf (mailbox-job-written job) files)
        (case outcome
          (:timeout (push (format nil "Reading was stopped after ~:d seconds (*outlook-mailbox-seconds*): ~
                                       what was read by then is listed"
                                  *outlook-mailbox-seconds*)
                          (mailbox-job-notes job)))
          (:full (push (format nil "Reading was stopped once ~a were written (*outlook-mailbox-space*), or the drive ~
                                    of the cache directory had less than 1 GB free: what was read by then is listed"
                               (octets-words (mailbox-job-octets job)))
                       (mailbox-job-notes job)))
          (:stopped (push (format nil "Reading stopped after ~:d of ~:d items: ~a. What was written by then is listed"
                                  (mailbox-job-items-read job) (or (mailbox-job-reading job) 0) why)
                          (mailbox-job-notes job))))
        (when problems
          (push (format nil "Found wrong in the mailbox, which was read as far as it could be: ~{~a~^; ~}" problems)
                (mailbox-job-notes job)))
        outcome))))

(defun mailbox-work (job)
  "What a job reading a mailbox does: the mailbox opened once, a link at its
path refused, and found to be the file its visit judged (MAILBOX-JOB-SEEN);
summed through that descriptor (MAILBOX-SUM); what was written of it before,
kept in the cache, listed when it is there (FIND-CACHE-ENTRY); else that
very file read and its items written afresh into a new entry of the cache
\(MAILBOX-READ-AFRESH), noted as whole once all are. Each is then read for
the list (MAILBOX-LIST)."
  (let* ((path (mailbox-job-path job))
         (buffer (job-buffer job))
         (origin (remote-copy-origin path)))
    (when origin
      (mailbox-hold-copy job origin))
    ;; refused when another file was put in its place since its visit judged it
    (call-with-file
     path
     (lambda (fd name size)
       (declare (ignore name size))
       (setf (mailbox-job-stamp job) (fd-stamp fd path)
             (mailbox-job-size job) (stamp-size (mailbox-job-stamp job))
             (mailbox-job-sum job) (or (mailbox-sum job fd path) (error "The mailbox could not be read")))
       (let* ((key (mailbox-cache-key (mailbox-job-sum job)))
              (kept (and (mailbox-listed-origin (nth-value 1 (find-cache-entry "mailbox" key)))
                         (find-cache-entry "mailbox" key :user buffer))))
         (if kept
             (progn (mailbox-take-entry job kept)
                    (push "Listed from what was written when the mailbox was last read, kept in the cache"
                          (mailbox-job-notes job))
                    (mailbox-list job))
             (let ((entry (make-cache-entry "mailbox" key (native-file-name (or origin path)) :user buffer)))
               (mailbox-take-entry job entry)
               (let ((outcome (mailbox-read-afresh job origin fd)))
                 (mailbox-list job)
                 (when (eq outcome :done)
                   (note-cache-entry entry :facts (format nil "listed ~a" (or origin path)))))))))
     :follow nil :same-as (mailbox-job-seen job))))

(defun mailbox-listed-files (files)
  "Those of FILES, a folder's as MAILBOX-FOLDERS gives them, that are items of
the mailbox, each (NAME . MSG): MSG the name of the same message written as
an Outlook item beside it, or NIL. An Outlook item beside a message of the
same number is not an item of its own, nor a file whose name begins with a
full stop, such as *MAILBOX-PATH-FILE*."
  (let ((names (make-hash-table :test 'equal)))
    (dolist (file files) (setf (gethash (car file) names) t))
    (loop for (name) in files
          for type = (native-file-type name)
          for msg = (format nil "~a.msg" (native-file-stem name))
          unless (or (and (string-equal type "msg") (gethash (format nil "~a.eml" (native-file-stem name)) names))
                     (uiop:string-prefix-p "." name))
            collect (cons name (and (string-equal type "eml") (gethash msg names) msg)))))

(defun mailbox-folder-path (directory relative)
  "The path of the folder written at RELATIVE in DIRECTORY, as its list shows
it: what its *MAILBOX-PATH-FILE* holds, the path whole, when it has one, else
RELATIVE without its last /."
  (let* ((file (path-join directory (concatenate 'string relative *mailbox-path-file*)))
         (whole (and (eq (path-kind file :follow nil) :regular)
                     (ignore-errors (file-text file :limit 65536)))))
    (if (and whole (plusp (length whole))) whole (string-right-trim "/" relative))))

(defun mailbox-list (job)
  "Read each item written in JOB's directory for the list (MAILBOX-ITEM-OF,
MAILBOX-LISTED-FILES), the folders in order, counting them as they are read,
each message's text for a search of the list written into JOB's text directory;
JOB's FOLDERS then hold them, its THREADS the tables of their links
\(MAILBOX-THREAD-TABLES), and its LABELS the labels kept for the mailbox when
its sum was made (MAILBOX-READ-LABELS)."
  (let* ((directory (mailbox-job-directory job))
         (folders (multiple-value-bind (written more) (mailbox-folders directory :limit *outlook-mailbox-most-files*)
                    (when more
                      (push (format nil
                                    "Only the first ~:d files written are listed (*outlook-mailbox-most-files*)"
                                    *outlook-mailbox-most-files*)
                            (mailbox-job-notes job)))
                    (let ((found (reduce #'+ written :key (lambda (folder) (length (cdr folder)))))
                          (said (mailbox-job-written job)))
                      (when (and said (not more) (/= said found))
                        (push (format nil "The reader wrote ~:d files, but ~:d are there to be listed" said found)
                              (mailbox-job-notes job))))
                    (loop for (relative . files) in written
                          for listed = (mailbox-listed-files files)
                          when listed collect (cons relative listed))))
         (texts (mailbox-job-text-directory job)))
    (setf (mailbox-job-total job) (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
    (flet ((item-of (relative name msg)
             (job-check-stop)
             (incf (mailbox-job-listed job))
             (let* ((msg (and msg (path-join directory (concatenate 'string relative msg))))
                    (item (mailbox-item-of (path-join directory (concatenate 'string relative name)) name
                                           :text-path (and texts
                                                           (path-join texts (princ-to-string (mailbox-job-listed job))))
                                           :folder (string-right-trim "/" relative) :msg msg
                                           :field-names (mailbox-job-field-names job))))
               (setf (mailbox-item-msg item) msg)
               item)))
      (setf (mailbox-job-folders job)
            (loop for (relative . files) in folders
                  collect (cons (mailbox-folder-path directory relative)
                                (loop for (name . msg) in files collect (item-of relative name msg))))
            (mailbox-job-threads job) (mailbox-thread-tables (mailbox-job-folders job))
            (mailbox-job-labels-name job) (and (mailbox-job-sum job) (mailbox-labels-name (mailbox-job-sum job)))))
    (when (mailbox-job-labels-name job)
      (multiple-value-bind (table meta kept) (mailbox-read-labels (mailbox-job-labels-name job))
        (setf (mailbox-job-labels job) table)
        (when kept (mailbox-note-mailbox job meta))))
    (mailbox-number-identities (mailbox-job-folders job))))

(defparameter *mailbox-columns*
  '((" " :width 1) ("Sent" :width 16) ("From" :width 30) ("Att" :width 3 :right t) ("Subject"))
  "The columns of a mailbox's list, as TABLE-HEADING-PIECES takes them: the
mark (>), when it was sent, whom from, its attachments and its subject.")

(defparameter *mailbox-labels-column* '("Labels" :width 20)
  "The column of a mailbox's list showing each message's labels, before its
subject, while any message of the mailbox carries one.")

(defun mailbox-folder-title (folder)
  "What a mailbox's list calls FOLDER, the path it was written at."
  (if (plusp (length folder)) (one-line folder) "(the mailbox itself)"))

(defun mailbox-cells (item mark labels)
  "The cells of the line of ITEM, marked MARK, in a mailbox's list: its mark,
when it was sent, whom from, its attachments, its LABELS, a list, unless that
is :NONE, and its subject."
  (append (list (list-mark-sign mark)
                (cons (or (short-time-words (mailbox-item-sent item)) "") :list-date)
                (or (mailbox-item-from item) "")
                (if (plusp (mailbox-item-attachments item)) (princ-to-string (mailbox-item-attachments item)) ""))
          (and (listp labels) (list (cons (format nil "~{~a~^, ~}" labels) :list-label)))
          (list (cons (or (mailbox-item-subject item) "(no subject)") (list-mark-face mark)))))

(defun filled-words (text)
  "TEXT with its words filled into lines no longer than *HELP-COLUMN*
\(WRITE-FILLED), so that a mailbox's words above its list read whole in a
window of any width."
  (with-output-to-string (out)
    (write-filled out "" text)))

(defun mailbox-render (buffer job &optional (point :first))
  "Fill BUFFER, a mailbox's list, with what JOB read: a line on the mailbox,
then each folder, a section that folds, and its items, their marks kept
\(SECTIONED-PIECES); point on the first item, or with POINT :keep where it
was. Its searches narrow it, matching each message's headers and contents
\(MAILBOX-ITEM-SEARCH-TEXT), the folders' lines kept over the items shown."
  (let* ((folders (mailbox-job-folders job))
         (labelled (and (mailbox-job-labels job) (plusp (hash-table-count (mailbox-job-labels job)))))
         (count (reduce #'+ folders :key (lambda (f) (length (cdr f)))))
         (intro (list (cons (format nil
                                    "Outlook mailbox ~a, ~a: ~:d item~:p in ~:d folder~:p~
                                     ~:[~;, deleted items included~]~%"
                                    (native-file-name (mailbox-job-path job))
                                    (octets-words (or (ignore-errors (stamp-size (path-stamp (mailbox-job-path job))))
                                                      0))
                                    count (length folders) *outlook-include-deleted*)
                            :heading)
                      (format nil
                              "~a~%"
                              (filled-words
                               (substitute-command-keys
                                (format nil
                                        "\\<outlook-mailbox-mode>\\`RET' or \\`l' opens the item on the line, ~
                                         a folder's line folds; ~
                                         \\<outlook-view-mode>\\[outlook-view-pages] in a message shows it as ~
                                         it looks, \\[quit-window] comes back; ~
                                         \\<outlook-mailbox-mode>\\[regex-search-forward] narrows the list to ~
                                         the items whose headers or contents match; \\[list-mark-entry] marks, ~
                                         \\[outlook-export] copies out."))))
                      (format nil "~a~%" (filled-words (format nil "~a." (mailbox-labels-place-words))))))
         (partial (loop for (nil . items) in folders sum (count-if-not #'mailbox-item-whole items)))
         (notes (loop for note in (append (reverse (mailbox-job-notes job))
                                          (and (plusp partial)
                                               (list (format nil
                                                             "~:d message~:p too large to be searched whole: ~
                                                              only the first ~:d characters, or the headers of one ~
                                                              larger than ~a, are searched ~
                                                              (*outlook-search-text-limit*, *outlook-largest-file*)"
                                                             partial
                                                             *outlook-search-text-limit*
                                                             (octets-words *outlook-largest-file*)))))
                      collect (cons (format nil "~a~%" (filled-words note)) :warning))))
    (set-search-filtering buffer :text (lambda (entry where) (mailbox-item-part-text job (entry-member entry) where))
                                 :nouns "items" :searches t
                                 :parts (mailbox-field-names (mailbox-job-field-names job))
                                 :light-part #'mailbox-light-field)
    (setf (buffer-local buffer 'mailbox-job) job)
    (let* ((before (append intro notes (list (string #\Newline))))
           (first-line (+ 2 (count #\Newline (pieces-text before)))))
      (render-table-list
       buffer (if labelled (append (butlast *mailbox-columns*) (list *mailbox-labels-column*) (last *mailbox-columns*))
                  *mailbox-columns*)
       folders
       (lambda (item mark folder)
         (declare (ignore folder))
         (mailbox-cells item mark (if labelled (mailbox-item-labels job item) :none)))
       :sections t :section-title #'mailbox-folder-title :keep-marks nil
       :before before
       ;; point on the first item, under its folder's line
       :point (if (eq point :first) (+ first-line (if (some #'cdr folders) 1 0)) point)
       :refill (lambda (buffer) (mailbox-render buffer job :keep))))))

(defun mailbox-item-part-text (job item part)
  "What a filter of the list of the mailbox JOB read looks through in ITEM, a
message, for PART: with PART NIL its headers and contents
\(MAILBOX-ITEM-SEARCH-TEXT); with your label, its labels, each on a line; with the
name of another field, that field's values, each on a line
\(MAILBOX-ITEM-FIELD-TEXT). NIL for a message without that part."
  (cond ((null part) (mailbox-item-search-text item))
        ((string= part "your label")
         (let ((labels (and (mailbox-item-p item) (mailbox-item-labels job item))))
           (and labels (format nil "~{~a~^~%~}" labels))))
        (t (mailbox-item-field-text item part))))

(defmethod job-take-in ((job mailbox-job))
  "List what the mailbox held, or say why it could not be read."
  (show-reading-result job (mailbox-job-path job) (lambda (buffer) (mailbox-render buffer job))))

(defun mailbox-buffer (native)
  "The buffer listing the mailbox at NATIVE: the one there is, else a new one.
The mailbox is read for it in the background (MAILBOX-WORK). The messages
written of it, and the text searched in each, are kept for you alone in an
entry of the cache in mailboxes/ of the cache directory, used by the list while
it is open and kept after to list the mailbox again (*CACHE-KEEP*). Its
searches narrow it once it is listed (MAILBOX-RENDER)."
  (let ((name (native-file-name native)))
    (reading-buffer '*mailboxes* (lambda (b) (buffer-local b 'mailbox-path)) native
                    (format nil "~a (mailbox)" name)
                    (format nil "Reading ~a...~%" name)
                    'outlook-mailbox-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'mailbox-path) native
                            (list-preview-p buffer) *outlook-preview*)
                      (let ((job (make-mailbox-job buffer native nil)))
                        (setf (mailbox-job-seen job) (file-view-stamp))
                        (start-job-thread job #'mailbox-work :name "tuatara mailbox"))))))

(defun mailbox-visit (path show)
  "Show an Outlook mailbox visited at PATH (a .pst or .ost, by its contents)
as the list of what it holds (MAILBOX-BUFFER), with SHOW, a function of the
buffer. Returns the buffer, or NIL for a file of another kind."
  (let ((native (file-view-path path)))
    (when (and native (eq :pst (nth-value 1 (file-kind native))))
      (file-view-show native (remove-if-not #'live-buffer-p *mailboxes*) (lambda (b) (buffer-local b 'mailbox-path))
                      (lambda () (mailbox-buffer native)) show))))

(define-command outlook-back-to-directory ()
  "Close the list of the mailbox, after asking, and list its directory with rimu,
point on the mailbox: the messages written of it stay in the cache, as
when it is closed any other way, until unused for *CACHE-KEEP*."
  (let* ((buffer (current-buffer))
         (path (buffer-local buffer 'mailbox-path)))
    (when (and path (y-or-n-p* (format nil "Close ~a and go back to its directory in rimu?" (buffer-name buffer))))
      (rimu-show-file path)
      (kill-buffer buffer))))

(defmethod list-entry-noun ((mode outlook-mailbox-mode))
  "A message of a mailbox, as the list's words for what is marked and acted
on call it: 3 messages marked."
  "message")

(defmethod visit-entry ((mode outlook-mailbox-mode) list item other)
  "Open ITEM of the mailbox listed in LIST: a message as one, read only, the
matches of the filters narrowing the list lit in it, and at its end the
attachments of the earlier messages of its conversation the mailbox holds
\(MAILBOX-EARLIER-ITEMS); a contact or an appointment as its text, safely;
with OTHER in another window, the list staying where it is."
  (let ((show (if other (lambda (buffer) (pop-to-buffer buffer :other t)) (lambda (buffer) (show-buffer buffer))))
        (job (buffer-local list 'mailbox-job)))
    (if (mailbox-item-message item)
        (let ((view (mail-view-buffer (mailbox-item-path item)
                                      :safe t :name (mailbox-item-subject item) :item (mailbox-item-msg item)
                                      :earlier (and job (mailbox-job-threads job)
                                                    (loop for (earlier . how)
                                                            in (mailbox-earlier-items (mailbox-job-threads job) item)
                                                          collect (cons (mailbox-item-path earlier)
                                                                        (mailbox-earlier-words earlier how)))))))
          (funcall show view)
          (light-search-filters list view))
        (visit-path (mailbox-item-path item) :safe t :show show))))

(defmethod preview-entry ((mode outlook-mailbox-mode) list item)
  "ITEM of the mailbox listed in LIST, beside the list: a message read in the
background into a buffer of its own for the preview, as a message is shown,
nothing in it run and nothing it points to fetched, the matches of the
list's filters lit in it; a contact or an appointment said to be one."
  (if (mailbox-item-message item)
      (let ((view (mail-view-buffer (mailbox-item-path item) :safe t :name (mailbox-item-subject item) :preview t
                                                             :item (mailbox-item-msg item))))
        (light-search-filters list view)
        (values view t))
      (values (side-preview-note (native-file-name (mailbox-item-path item))
                                 (substitute-command-keys
                                  (concatenate 'string "A contact or an appointment: "
                                               "\\<outlook-mailbox-mode>\\[list-visit] opens it as its text.")))
              t)))

(defmethod entry-preview-width ((mode outlook-mailbox-mode) list)
  "The columns of a mailbox's list, a subject's first words among them."
  (declare (ignore list))
  80)

;;; Copied out

(defun mailbox-export-name (item type)
  "The name a copy of ITEM, a message, takes copied out as TYPE (\"eml\" or
\"msg\"): when it was sent and its subject, a / or \\ in it made -, as a
file's name may hold them (ATTACHMENT-FILE-NAME), else the number it was
written under, cut to the 255 octets a name may take (FIT-NAME); any other item
keeps its own name."
  (if (mailbox-item-message item)
      (let ((subject (mailbox-item-subject item)) (sent (mailbox-item-sent item)))
        (fit-name (format nil "~a.~a"
                          (attachment-file-name (substitute-if #\- (lambda (c) (find c "/\\"))
                                                               (format nil "~@[~a ~]~@[~a~]"
                                                                       (and sent (format-time "%F %H%M" sent))
                                                                       subject))
                                                (native-file-stem (mailbox-item-path item)))
                          type)))
      (native-file-name (mailbox-item-path item))))

(define-record (mailbox-export-job (:include job)
                               (:constructor make-mailbox-export-job
                                   (buffer items directory type &optional name folders))
                               (:copier nil))
  "ITEMS of the mailbox listed in BUFFER being copied out into DIRECTORY as
TYPE (\"eml\" or \"msg\", a file each, or \"mbox\" or \"pst\", all in
one file called NAME), a job: COPIED of them so far, and NOT-COPIED, those
that could not be, each (ITEM . WHY). WRITTEN is the mbox or pst file once
made. FOLDERS, for a pst file, is the folders the items go in, each (NAMES
. ITEMS) as WRITE-PST takes them."
  (items nil :documentation "The items of the mailbox listed in the buffer that MAILBOX-EXPORT-JOB copies out.")
  (directory nil :documentation "The directory the items are copied into.")
  (type nil
        :documentation "What the items are copied as: \"eml\" or \"msg\", a file each, or \"mbox\" or \"pst\", all in
one file.")
  (name nil :documentation "The name of the one file for \"mbox\" or \"pst\".")
  (folders nil
           :documentation "For a pst file, the folders the items go in, each (NAMES . ITEMS) as WRITE-PST takes them.")
  (copied 0 :documentation "How many items have been copied so far.")
  (not-copied '() :documentation "The items that could not be copied, each (ITEM . WHY).")
  (written nil :documentation "The mbox or pst file once made."))

(define-job-words mailbox-export-job
  :doing "Copying messages out"
  :description (format nil "Copying ~:d message~:p out of a mailbox into ~a~:[ as .~a~;~*~]"
                       (length (mailbox-export-job-items job))
                       (or (and (mailbox-export-job-name job)
                                (path-join (mailbox-export-job-directory job) (mailbox-export-job-name job)))
                           (mailbox-export-job-directory job))
                       (mailbox-export-job-name job) (mailbox-export-job-type job))
  :verb "copy the messages out"
  :left-words (format nil "~:d copied" (mailbox-export-job-copied job)))

(defmethod job-percent ((job mailbox-export-job))
  "How many of the messages have been copied out, in per cent."
  (running-percent (+ (mailbox-export-job-copied job) (length (mailbox-export-job-not-copied job)))
                   (max 1 (length (mailbox-export-job-items job)))))

(defun mail-address-in (text)
  "The first address in TEXT, a header's value, written name@place: the run of
characters around its first @ that has some on either side, up to white
space, <, > or \" each way; NIL when there is none."
  (flet ((allowed (c) (not (member c '(#\< #\> #\" #\Space #\Tab #\Newline #\Return #\Page)))))
    (loop for at = (position #\@ text) then (position #\@ text :start (1+ at))
          while at
          do (let ((start (or (position-if-not #'allowed text :end at :from-end t) -1))
                   (end (or (position-if-not #'allowed text :start (1+ at)) (length text))))
               (when (and (< (1+ start) at) (> end (1+ at)))
                 (return (subseq text (1+ start) end)))))))

(defun mbox-from-line (part sent)
  "The line an mbox file writes before a message, PART (MIME-READ) sent at
SENT, a universal time or NIL: From, the address it came back to or came
from (MAILER-DAEMON for none), and the date in universal time as asctime
writes it, then a line feed."
  (let* ((header (or (mime-header part "return-path") (mime-header part "from") ""))
         (address (or (mail-address-in header) "MAILER-DAEMON")))
    (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time (or sent 0) 0)
      (format nil "From ~a ~a ~a ~2d ~2,'0d:~2,'0d:~2,'0d ~d~%"
              address (svref *short-weekday-names* (mod (1+ weekday) 7))
              (aref *short-month-names* (1- month)) day hour minute second year))))

(defun mbox-write-message (fd path octets)
  "Write OCTETS, a message, to the mbox file open as FD at PATH as mboxrd
does: each line that begins with From and a space after any number of >
written with one > more, a line feed added at the end when it has none, and
an empty line after it."
  (let ((run 0) (length (length octets)))
    (loop with start = 0
          while (< start length)
          do (let ((text (or (position 62 octets :start start :test-not #'eql) length))
                   (end (let ((lf (position 10 octets :start start))) (if lf (1+ lf) length))))
               (when (and (< text end) (octets-at-p octets "From " text))
                 (write-octets fd octets :start run :end start :path path)
                 (write-octets fd (string-octets ">") :path path)
                 (setf run start))
               (setf start end)))
    (write-octets fd octets :start run :end length :path path)
    (write-octets fd (string-octets (if (and (plusp length) (= 10 (aref octets (1- length))))
                                        (string #\Newline)
                                        (format nil "~%~%")))
                  :path path)))

(defun mailbox-export-mbox-work (job)
  "Collect JOB's items into one new mbox file in its directory, named NAME,
or NAME with (2) and so on where that is taken (CALL-WITH-FREE-FILE): each
message as written of the mailbox after its From line (MBOX-FROM-LINE,
MBOX-WRITE-MESSAGE), noting those that are no message or cannot be read. The
file is deleted when the job is stopped or fails."
  (call-with-free-file
   (mailbox-export-job-directory job) (mailbox-export-job-name job)
   (lambda (fd path)
     (setf (mailbox-export-job-written job) path)
     (let ((done nil))
       (unwind-protect
            (progn
              (dolist (item (mailbox-export-job-items job))
                (job-check-stop)
                (if (not (mailbox-item-message item))
                    (push (cons item "not a message") (mailbox-export-job-not-copied job))
                    (let ((octets (handler-case
                                      (call-with-file (mailbox-item-path item)
                                                      (lambda (fd file size)
                                                        (read-fd-octets fd :path file :size-hint (1+ size))))
                                    (file-system-error (e)
                                      (push (cons item (princ-to-string e)) (mailbox-export-job-not-copied job))
                                      nil))))
                      (when octets
                        (let ((head (mime-read octets :end (min (length octets) 65536))))
                          (write-octets fd (string-octets (mbox-from-line head (mailbox-item-sent item))) :path path))
                        (mbox-write-message fd path octets)
                        (incf (mailbox-export-job-copied job))))))
              (setf done t))
         (unless done
           (ignore-errors (remove-file path))
           (setf (mailbox-export-job-written job) nil)))))))

(defun mailbox-pst-message (item)
  "ITEM of a mailbox as a PST-MESSAGE to be written in a new mailbox: the
Outlook item written beside it as the mailbox was read, every property
kept, its named ones with what they stand for (MSG-PST-MESSAGE); an error
when it is no message or no Outlook item was written of it."
  (cond ((not (mailbox-item-message item)) (error "not a message"))
        ((null (mailbox-item-msg item)) (error "no Outlook item was written of it"))
        (t (let ((cfb (cfb-read (file-octets (mailbox-item-msg item)))) (*msg-items* '()))
             (msg-pst-message cfb (aref (cfb-entries cfb) 0))))))

(defun mailbox-export-pst-work (job)
  "Write JOB's items into one new Outlook mailbox in its directory, named
NAME, or NAME with (2) and so on where that is taken (CALL-WITH-FREE-FILE),
in JOB's folders (WRITE-PST): each message read from the Outlook item
written of it (MAILBOX-PST-MESSAGE), noting those that are no message or
cannot be read. The file is deleted when the job is stopped or fails."
  (call-with-free-file
   (mailbox-export-job-directory job) (mailbox-export-job-name job)
   (lambda (fd path)
     (setf (mailbox-export-job-written job) path)
     (let ((done nil))
       (unwind-protect
            (progn
              (write-pst fd path (native-file-stem path) (mailbox-export-job-folders job)
                         :each (lambda (item)
                                 (job-check-stop)
                                 (handler-case (prog1 (mailbox-pst-message item)
                                                 (incf (mailbox-export-job-copied job)))
                                   (error (e)
                                     (push (cons item (princ-to-string e)) (mailbox-export-job-not-copied job))
                                     nil))))
              (setf done t))
         (unless done
           (handler-case (remove-file path) (file-system-error () nil))
           (setf (mailbox-export-job-written job) nil)))))))

(defun mailbox-export-work (job)
  "Copy each of JOB's items out into its directory, a new file each
\(MAILBOX-EXPORT-NAME, COPY-TO-FREE-NAME), as JOB's type: an Outlook item
from the one written beside the message; noting those that cannot be.
For the type mbox, collect them into one file (MAILBOX-EXPORT-MBOX-WORK); for
pst, write them into a new mailbox (MAILBOX-EXPORT-PST-WORK)."
  (when (string= (mailbox-export-job-type job) "mbox")
    (return-from mailbox-export-work (mailbox-export-mbox-work job)))
  (when (string= (mailbox-export-job-type job) "pst")
    (return-from mailbox-export-work (mailbox-export-pst-work job)))
  (let ((directory (mailbox-export-job-directory job)) (type (mailbox-export-job-type job)))
    (dolist (item (mailbox-export-job-items job))
      (job-check-stop)
      (let ((from (if (and (string= type "msg") (mailbox-item-message item))
                      (mailbox-item-msg item)
                      (mailbox-item-path item))))
        (handler-case
            (cond ((null from) (error "no Outlook item was written of it"))
                  ((copy-to-free-name from directory (mailbox-export-name item type) :cancel (job-cancel job))
                   (incf (mailbox-export-job-copied job)))
                  (t (job-check-stop)))
          (error (e) (push (cons item (princ-to-string e)) (mailbox-export-job-not-copied job))))))))

(defmethod job-take-in ((job mailbox-export-job))
  "Say how many messages were copied out and where, and which could not be."
  (let ((missed (reverse (mailbox-export-job-not-copied job))))
    (message "~:[~a~;~*Copied ~:d message~:p into ~a~]~@[; not copied: ~{~a~^, ~}~]"
             (eq (job-status job) :done) (ignore-errors (job-ending-words job))
             (mailbox-export-job-copied job)
             (or (mailbox-export-job-written job) (mailbox-export-job-directory job))
             (and missed (loop for (item . why) in missed
                               collect (format nil "~a (~a)" (mailbox-item-name item) why))))))

(defun mailbox-export-items (list)
  "The items of LIST, a mailbox's list, a command copying out acts on: with
the menu's switch All shown on, every item its filters leave
\(ENTRIES-LEFT-BY-FILTERS), those of folded folders too, as folding hides
nothing from them; else those marked, or with none marked the one on the
line. NIL, said, while the filters are still worked out."
  (let ((job (buffer-local list 'mailbox-job)))
    (if (and job (member "--shown" (menu-arguments) :test #'string=))
        (let ((left (entries-left-by-filters list)))
          (case left
            ((t) (loop for (nil . items) in (mailbox-job-folders job) append items))
            (:working (message "The list's filters are still being worked out: try again once they are") nil)
            (t (remove-if-not #'mailbox-item-p (mapcar #'entry-member left)))))
        (remove-if-not #'mailbox-item-p (list-acted-on list)))))

(defun mailbox-export-directory (prompt)
  "Ask with PROMPT for a directory to write in, made when it is not there: its
path, or NIL, said, when none is given, nothing may be written there
\(WRITING-REFUSAL) or it cannot be made."
  (let* ((directory (or (read-native-path prompt :writing t)
                        (progn (message "No directory given: nothing copied") nil)))
         (refusal (and directory (writing-refusal (if (path-kind directory) directory (parent-path directory))))))
    (when refusal
      (message "~a: nothing copied" refusal)
      (setf directory nil))
    (when (and directory (null (path-kind directory :follow nil)))
      (handler-case (make-directory directory)
        (error (e) (message "~a: nothing copied" e) (setf directory nil))))
    (cond ((null directory) nil)
          ((not (eq :directory (path-kind directory)))
           (message "~a is not a directory: nothing copied" directory)
           nil)
          (t directory))))

(defun mailbox-export-as (type)
  "Copy the messages marked in the mailbox's list, or with none marked the
one on the current line, or with the menu's switch All shown on every one
its filters leave (MAILBOX-EXPORT-ITEMS), out into a directory asked for, as
TYPE, \"eml\" or \"msg\" (MAILBOX-EXPORT-WORK, in the background)."
  (let* ((list (current-buffer))
         (items (mailbox-export-items list)))
    (if (null items)
        (unless (member "--shown" (menu-arguments) :test #'string=) (message "No message on this line"))
        (let* ((words (list-question nil "message" items :name-of #'mailbox-item-name))
               (directory (mailbox-export-directory (format nil "Copy ~a as .~a into: " words type))))
          (when directory
            (start-job-thread (make-mailbox-export-job list items directory type) #'mailbox-export-work
                              :name "tuatara mailbox export")
            (message "Copying ~a out into ~a as .~a..." words directory type))))))

(define-command outlook-export-eml ()
  "Copy the messages marked in the mailbox's list
\(\\<outlook-mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, out into a directory asked for, as saved mail (.eml). A directory
not there yet is made. Each is a new file named by when it was sent and its
subject, (2) and so on added where that name is taken; nothing there is
overwritten. They are copied in the background, as they were written; the
mailbox itself is not touched. A contact or an appointment is copied as it is."
  (mailbox-export-as "eml"))

(define-command outlook-export-msg ()
  "Copy the messages marked in the mailbox's list
\(\\<outlook-mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, out into a directory asked for, as Outlook items (.msg), as
\\[outlook-export-eml] copies them as saved mail."
  (mailbox-export-as "msg"))

(define-command outlook-export-mbox ()
  "Collect the messages marked in the mailbox's list
\(\\<outlook-mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, into one mbox file asked for, the format most mail programs
read: each message as it was written, after a From line, a line of it
beginning with From (after any number of >) written with one > more, as
mboxrd does. A name taken gets (2) and so on; nothing is overwritten. It is
written in the background; the mailbox itself is not touched. Contacts and
appointments are left out."
  (let* ((list (current-buffer))
         (items (mailbox-export-items list)))
    (if (null items)
        (message "No message on this line")
        (let* ((words (list-question nil "message" items :name-of #'mailbox-item-name))
               (path (read-native-path (format nil "Collect ~a into the mbox file: " words) :writing t))
               (name (and path (native-file-name path)))
               (refusal (and path (writing-refusal (parent-path path)))))
          (cond ((or (null path) (zerop (length name))) (message "No file named: nothing collected"))
                ((not (eq :directory (path-kind (parent-path path))))
                 (message "~a is not a directory: nothing collected" (parent-path path)))
                (refusal (message "~a: nothing collected" refusal))
                (t (start-job-thread (make-mailbox-export-job list items (parent-path path) "mbox" name)
                                     #'mailbox-export-work :name "tuatara mailbox export")
                   (message "Collecting ~a into ~a..." words path)))))))

(defun mailbox-export-folders (list items)
  "ITEMS of the mailbox listed in LIST in the folders they are listed in, in
the order of the list, as WRITE-PST takes them: each (NAMES . ITEMS), NAMES
the path of the folder below the top folder it was written in."
  (let ((chosen (make-hash-table :test 'eq)) (job (buffer-local list 'mailbox-job)))
    (dolist (item items) (setf (gethash item chosen) t))
    (loop for (folder . listed) in (and job (mailbox-job-folders job))
          for those = (remove-if-not (lambda (item) (gethash item chosen)) listed)
          when those collect (cons (rest (uiop:split-string folder :separator "/")) those))))

(define-command outlook-export-pst ()
  "Write the messages marked in the mailbox's list
\(\\<outlook-mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, into a new Outlook mailbox (.pst) asked for, in the folders
they are listed in: each with every property, recipient and attachment the
Outlook item written of it holds, its named properties among them. .pst is
added to a name without it; a name taken gets (2) and so on, and nothing is
overwritten, nor written in an evidence directory. It is written in the
background; the mailbox itself is not touched. Contacts and appointments are
left out."
  (let* ((list (current-buffer))
         (items (mailbox-export-items list)))
    (if (null items)
        (unless (member "--shown" (menu-arguments) :test #'string=) (message "No message on this line"))
        (let* ((words (list-question nil "message" items :name-of #'mailbox-item-name))
               (path (read-native-path (format nil "Write ~a into the new Outlook mailbox: " words) :writing t))
               (name (and path (native-file-name path)))
               (name (if (and name (plusp (length name)) (not (string-equal (native-file-type name) "pst")))
                         (concatenate 'string name ".pst")
                         name))
               (refusal (and path (writing-refusal (parent-path path)))))
          (cond ((or (null path) (zerop (length name))) (message "No file named: nothing written"))
                ((not (eq :directory (path-kind (parent-path path))))
                 (message "~a is not a directory: nothing written" (parent-path path)))
                (refusal (message "~a: nothing written" refusal))
                (t (start-job-thread (make-mailbox-export-job list items (parent-path path) "pst" name
                                                              (mailbox-export-folders list items))
                                     #'mailbox-export-work :name "tuatara mailbox export")
                   (message "Writing ~a into ~a..." words (path-join (parent-path path) name))))))))

(define-menu outlook-export (:title "Copy out")
  "Open the menu of copying the messages marked in the mailbox's list, or with
none marked the one on the current line, out: \\`e' as saved mail (.eml) and
\\`m' as Outlook items (.msg) into a directory, \\`b' into one mbox file,
\\`p' into a new Outlook mailbox (.pst), the directory or the file asked for
then. \\`a' switches to every message the list's filters leave, marked or
not. \\`C-h' there lists every action."
  ("Which"
   ("a" "All shown, not only those marked" :switch "--shown"))
  ("Copy out as"
   ("e" "Saved mail (.eml)" outlook-export-eml)
   ("m" "Outlook items (.msg)" outlook-export-msg)
   ("b" "One mailbox file (.mbox)" outlook-export-mbox)
   ("p" "Outlook mailbox (.pst)" outlook-export-pst)))
