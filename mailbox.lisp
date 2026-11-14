;;;; mailbox.lisp --- Outlook mailboxes (.pst, .ost) read by readpst, shut in, and listed.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A mailbox is read by readpst (libpst), shut in the sandbox: it sees a
;;;; copy of the mailbox alone, read only (the copy made by Tuatara, the
;;;; mailbox's access time left as it was), and writes each item as a separate message of
;;;; internet mail into a new directory for you alone in the cache directory, a
;;;; directory of messages for each folder of the mailbox. It runs in the
;;;; background, however large the mailbox, held to a time, to a space on the
;;;; drive and to the drive's free space; the folders and messages it wrote are
;;;; then listed, a line each, and opened as messages (view.lisp). As each
;;;; message is read for the list, the text a search of the list looks
;;;; through is written beside it, once: its headers, its text parts decoded
;;;; and its attachments' names, nothing else of an attachment decoded.
;;;; Tuatara's search filters narrow the list by it, matched in the
;;;; background. The mailbox itself is never changed; what readpst wrote goes
;;;; when its list is closed. readpst writes each message twice, as internet
;;;; mail (.eml) and as an Outlook item (.msg), so that those marked can be
;;;; copied out as either.

(in-package #:outlook)

(define-major-mode mailbox-mode sectioned-list-mode
  "An Outlook mailbox (personal storage table, .pst, or offline storage table,
.ost) read by readpst, shut in the sandbox, with its folders, and under each
its messages, a line each: when it was sent, whom from, how many attachments
it has and its subject.
\\<mailbox-mode>\\[list-visit] or \\`l' opens the message on the line, read
only, the matches of the list's filters lit in it. \\`P' there shows it as it
looks and \\`q' comes back. \\[list-visit-other] opens it in another window,
the list staying. \\[list-next-entry] and \\[list-previous-entry] go to the
next item and the one before. On a folder's line it folds or unfolds the
folder, as \\[list-toggle-fold] does, and \\[list-toggle-all-folds] folds or
unfolds them all.
\\[list-mark-entry] marks the message on the line (>), \\[list-unmark-entry]
takes its mark off, \\[list-unmark-all] takes every mark off, and
\\[list-toggle-marks] marks those not marked and unmarks those marked.
\\[mailbox-export] copies those marked, or the one on the line, out into a
directory, as saved mail (.eml) or as Outlook items (.msg), from a menu.
\[mailbox-label] puts labels of your own on them, shown on their lines and
kept for the mailbox between sessions, and \[mailbox-unlabel] takes them off;
\[mailbox-filter-labels] narrows the list to the messages carrying a label.
The message on the line is shown beside the list as the cursor moves, read as
any message is (*OUTLOOK-PREVIEW*); \\[list-toggle-preview] turns that off and
on.
\\[quit-window] closes the list, \\[mailbox-back-to-directory] too, after asking,
going back to its directory in rimu; the messages readpst wrote for it are
deleted with it.
\\[regex-search-forward] and \\[regex-search-backward] narrow the list as a regular
expression is typed to the messages whose headers, any of them, or contents
match: the text of each text part, decoded, and the names of the
attachments; \\<search>\\[search-exit] keeps the pattern as a filter, the next search
narrowing what it left, and \\[search-abort] lets the one typed go.
\\<mailbox-mode>\\[search-filter-show-records] narrows the list by one field of
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

(defstruct (mailbox-item (:copier nil))
  "An item of a mailbox as readpst wrote it. PATH is its file. MSG is the same
message written as an Outlook item, or NIL. MESSAGE is true for a message
(else a contact, an appointment, written as is). SENT is a universal time or
NIL. FROM and SUBJECT are as written to be read. ATTACHMENTS is how many.
TEXT-FILE is the file of what a search of the list looks through in it, or
NIL. WHOLE is NIL when that is only a part of it, the message being too
large. IDENTITY is what a message is known by from one reading of the
mailbox to the next (MAILBOX-MESSAGE-IDENTITY), or NIL. FIELDS-FILE is the
file of its fields (MAILBOX-MESSAGE-FIELDS), or NIL. THREAD is how it links to
the earlier messages of its conversation (MAILBOX-THREAD-OF), or NIL."
  path (msg nil) message sent from subject (attachments 0) (text-file nil) (whole t) (identity nil)
  (fields-file nil) (thread nil))

(setf (documentation 'mailbox-item-p 'function) "True when OBJECT is a MAILBOX-ITEM.")

(defun mailbox-readpst ()
  "The full path of readpst (*OUTLOOK-READPST-PROGRAM*) where the sandbox finds
it, or NIL."
  (sandbox-find-program *outlook-readpst-program*))

(defun mailbox-preview-words (name size)
  "What the preview says of the mailbox NAME, of SIZE octets."
  (format nil "~a is an Outlook mailbox, ~a.~%~%~
               ~:[readpst is not installed: install it (Fedora: libpst) to read it.~;~
               Opened (RET in a list), it is read by readpst, shut in the sandbox: ~
               its folders and messages listed~:[~;, deleted items among them~].~]~%"
          name (octets-words size) (mailbox-readpst) *outlook-include-deleted*))

;;; What readpst wrote

(defun mailbox-files (root function)
  "Call FUNCTION on what readpst wrote under ROOT, 64 directories deep: with
the directory's path from ROOT (\"\" for ROOT) and NIL as each directory is
entered, and with that path, the name and the size of each regular file in
it. Links, whatever they lead to, and other entries are passed over."
  (walk-directory root
                  (lambda (directory relative entry)
                    (declare (ignore directory))
                    (if (null entry)
                        (funcall function relative nil nil)
                        (destructuring-bind (name kind size own &rest rest) entry
                          (declare (ignore rest))
                          (when (eq own kind)
                            (case kind
                              (:directory t)
                              (:regular (funcall function relative name size) nil))))))
                  :max-depth 64))

(defun mailbox-folders (root &key (limit 1000000) (in-order t))
  "The folders under ROOT, a directory readpst wrote in, and ROOT itself,
each (RELATIVE . FILES): RELATIVE its path from ROOT (\"\" for ROOT), FILES
each (NAME . SIZE), its regular files, in the order of their numbers unless
IN-ORDER is NIL. Links and other entries are passed over; at most LIMIT
files, 64 folders deep, and true as a second value when there were more."
  (let ((found '()) (folders (make-hash-table :test 'equal)) (count 0) (more nil))
    (mailbox-files root
                   (lambda (relative name size)
                     (cond ((null name) (push (setf (gethash relative folders) (list relative)) found))
                           ((< count limit)
                            (incf count)
                            (push (cons name size) (cdr (gethash relative folders))))
                           (t (setf more t)))))
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

(defun mailbox-written (root)
  "How many regular files readpst has written under ROOT, all of them, and
their octets, as two values: counted as its folders are walked, 64 deep,
links passed over, nothing kept."
  (let ((files 0) (octets 0))
    (ignore-errors
     (mailbox-files root (lambda (relative name size)
                           (declare (ignore relative))
                           (when name (incf files) (incf octets size)))))
    (values files octets)))

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

(defun mailbox-item-of (path name &key text-path (folder "") msg field-names)
  "The MAILBOX-ITEM the file NAME at PATH, written by readpst in FOLDER, is: a
message (a name with no ending, or .eml) read for its date, sender, subject
and attachments counted, and known by its identity (MAILBOX-MESSAGE-IDENTITY);
any other as it is named. A message larger than *OUTLOOK-LARGEST-FILE* is read
for its headers alone. MSG is the same message as an Outlook item, or NIL.
With TEXT-PATH, a message's text for a search of the list
\(MAILBOX-SEARCH-TEXT) is written there, and its fields
\(MAILBOX-MESSAGE-FIELDS) beside it, TEXT-PATH.fields, new files for you
alone, the fields' names put in the table FIELD-NAMES."
  (let ((type (native-file-type name)))
    (if (not (or (null type) (string-equal type "eml")))
        (make-mailbox-item :path path :subject name)
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
                  (setf (mailbox-item-text-file item) (write-new-file text-path text)
                        (mailbox-item-whole item) (and whole (not large))))
                (let ((fields (mailbox-message-fields part msg)))
                  (when field-names
                    (loop for (field) in fields do (setf (gethash field field-names) t)))
                  (setf (mailbox-item-fields-file item)
                        (write-new-file (concatenate 'string text-path ".fields") (mailbox-fields-text fields))
                        (mailbox-item-thread item) (mailbox-thread-of part fields))))
              item)
          (error () (make-mailbox-item :path path :message t :subject (format nil "~a (could not be read)" name)))))))

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

(defstruct (mailbox-job (:include job)
                        (:constructor make-mailbox-job (buffer path directory &optional text-directory))
                        (:copier nil))
  "The mailbox at PATH being read for BUFFER, a job: first COPIED of SIZE
octets of it copied for readpst; then readpst writing into DIRECTORY, FILES of
OCTETS so far; then LISTED of TOTAL files read for the list, the text a
search of the list looks through in each message written into TEXT-DIRECTORY,
when there is one. FOLDERS, what it lists once done, each (NAME . ITEMS);
NOTES, what to say of how it went. SUMMED is a list holding how many octets
of the copy are summed so far, while they are; SUM is the mailbox's SHA-256 sum, or NIL
when it could not be made; LABELS, the labels on its messages, a table from
each one's identity to its labels (MAILBOX-READ-LABELS), or NIL without a sum.
LABELS-NAME is the name of their journal (MAILBOX-LABELS-NAME), taken as it
is listed. LABELS-WRITER is the job keeping them last started, and LABELS-PENDING the
changes made since it took them, (LINES . NAMES) as MAILBOX-KEEP-LABELS takes
them.
FIELD-NAMES is a table of the names of the fields of its messages
\(MAILBOX-MESSAGE-FIELDS), found as they are listed; THREADS, the tables
finding its messages by their links to earlier ones (MAILBOX-THREAD-TABLES),
made once they are."
  path directory text-directory (copied nil) (size 0) (files 0) (octets 0) (listed 0) (total nil) folders (notes '())
  (summed nil) (sum nil) (labels nil) (labels-name nil) (labels-writer nil) (labels-pending nil)
  (field-names (make-hash-table :test 'equal)) (threads nil))

(setf (documentation 'mailbox-job-p 'function) "True when OBJECT is a MAILBOX-JOB.")

(define-job-words mailbox-job
  :doing (cond ((mailbox-job-total job) "Listing the mailbox")
               ((mailbox-job-copied job) "Copying the mailbox")
               ((mailbox-job-summed job) "Summing the mailbox")
               (t "Reading the mailbox"))
  :description (format nil "Reading the mailbox ~a with readpst" (native-file-name (mailbox-job-path job)))
  :verb "read the mailbox"
  :left-words "nothing listed")

(defmethod job-percent ((job mailbox-job))
  "How far copying the mailbox or listing the messages has got; NIL while
readpst writes them."
  (let ((total (mailbox-job-total job)))
    (cond (total (running-percent (mailbox-job-listed job) total))
          ((mailbox-job-summed job)
           (running-percent (car (mailbox-job-summed job)) (max 1 (mailbox-job-size job))))
          ((mailbox-job-copied job) (running-percent (mailbox-job-copied job) (max 1 (mailbox-job-size job)))))))

(defmethod job-state-words ((job mailbox-job))
  "How far reading the mailbox has got: the per cent copied, the messages
readpst has written, then the per cent summed and listed."
  (if (or (mailbox-job-total job) (mailbox-job-copied job) (mailbox-job-summed job))
      (call-next-method)
      (format nil "Reading the mailbox: ~:d items, ~a"
              (mailbox-job-files job) (octets-words (mailbox-job-octets job)))))

(defun mailbox-copy-path (job)
  "Where JOB's mailbox is copied for readpst: beside its directory."
  (concatenate 'string (string-right-trim "/" (mailbox-job-directory job)) ".pst"))

(defun mailbox-copy (job copy)
  "Copy JOB's mailbox to COPY (MAILBOX-COPY-PATH) for readpst, read without
changing the mailbox's access time: refused when the drive would keep less than
1 gigabyte (GB) free, stopped when JOB is asked to stop. The caller deletes COPY
however it ends."
  (let ((size (stamp-size (path-stamp (mailbox-job-path job))))
        (free (ignore-errors (free-octets (parent-path copy)))))
    (when (and free (< free (+ size (expt 2 30))))
      (error "The drive of the cache directory has room for no copy of this mailbox (~a, and 1 GB to spare)"
             (octets-words size)))
    (setf (mailbox-job-size job) size (mailbox-job-copied job) 0)
    (unless (copy-regular-file (mailbox-job-path job) copy :cancel (job-cancel job)
                               :progress (lambda (done) (setf (mailbox-job-copied job) done)))
      (job-check-stop))
    (setf (mailbox-job-copied job) nil)))

(defun mailbox-count-seconds (files)
  "How many seconds apart what readpst has written is counted, FILES written
so far: each second at first, at most every thirty once there are many, as
each count looks at every file."
  (min 30 (max 1 (floor files 2000))))

(defun mailbox-run-readpst (job copy)
  "Run readpst on COPY, JOB's mailbox copied (MAILBOX-COPY), shut in, writing
into JOB's directory, and watch it: what it has written counted now and then
\(MAILBOX-COUNT-SECONDS), the drive's free room each second, and it stopped when
JOB is asked to stop, when it has written more than *OUTLOOK-MAILBOX-SPACE*, or
when the drive has less than 1 gigabyte (GB) free. Returns how it went
\(SANDBOX-RUN's second value) and what it said."
  (let* ((directory (mailbox-job-directory job))
         (stop (list nil)) (full nil) (result nil)
         ;; -j 0: readpst alone, no processes forked, which the sandbox would end, unfinished, as readpst ends
         (arguments (append '("-m" "-j" "0" "-b" "-q")
                            (and *outlook-include-deleted* '("-D"))
                            '("-o" "/tmp/out" "/tmp/mailbox.pst"))))
    (let ((thread (make-guarded-thread
                   (lambda ()
                     (handler-case
                         (multiple-value-list
                          (sandbox-run *outlook-readpst-program* arguments
                                       :directories (list (cons copy "/tmp/mailbox.pst"))
                                       :writable (list (cons (string-right-trim "/" directory) "/tmp/out"))
                                       :seconds *outlook-mailbox-seconds* :memory (* 2 1024 1024 1024)
                                       :output-limit nil :file-limit *outlook-mailbox-space* :cancel stop))
                       (error (e) (list nil :failed (princ-to-string e)))))
                   :name "tuatara readpst")))
      (loop with next-count = 1
            for second from 1
            do (multiple-value-bind (value ended) (thread-result thread 1)
                 (when ended
                   (setf result (or value (list nil :failed "it ended unexpectedly")))
                   (loop-finish)))
               (when (>= second next-count)
                 (multiple-value-bind (files octets) (mailbox-written directory)
                   (setf (mailbox-job-files job) files (mailbox-job-octets job) octets
                         next-count (+ second (mailbox-count-seconds files)))
                   (when (and (not full) (> octets *outlook-mailbox-space*))
                     (setf full
                           (format nil
                                   "readpst was stopped once it had written ~a (*outlook-mailbox-space*)"
                                   (octets-words octets))
                           (car stop) t))))
               (let ((free (ignore-errors (free-octets directory))))
                 (when (and (not full) free (< free (expt 2 30)))
                   (setf full "readpst was stopped: the drive of the cache directory has less than 1 GB free"
                         (car stop) t)))
               (when (car (job-cancel job)) (setf (car stop) t))))
    (job-check-stop)
    (destructuring-bind (output outcome said) result
      (declare (ignore output))
      (when full (push full (mailbox-job-notes job)))
      (values outcome said))))

(defun mailbox-sum (job copy)
  "Sum COPY, JOB's mailbox copied (MAILBOX-COPY), with SHA-256, as the
mailbox's labels are kept by (MAILBOX-READ-LABELS); NIL when it cannot be
read, and stopped when JOB is asked to stop."
  (setf (mailbox-job-summed job) (list 0))
  (unwind-protect (file-sha256 copy :cancel (job-cancel job) :done (mailbox-job-summed job))
    (setf (mailbox-job-summed job) nil)
    (job-check-stop)))

(defun mailbox-work (job)
  "What a job reading a mailbox does: readpst writes its items
\(MAILBOX-RUN-READPST) from a copy of the mailbox (MAILBOX-COPY), summed for
its labels (MAILBOX-SUM), then each is read for the list (MAILBOX-LIST)."
  (multiple-value-bind (outcome said) (let ((copy (mailbox-copy-path job)))
                                        (unwind-protect (progn (mailbox-copy job copy)
                                                               (setf (mailbox-job-sum job) (mailbox-sum job copy))
                                                               (mailbox-run-readpst job copy))
                                          (ignore-errors (remove-file copy))))
    (let ((directory (mailbox-job-directory job)))
      (case outcome
        (:done)
        (:unavailable (error "~a" said))
        (:timeout (push (format nil
                                "readpst was stopped after ~:d seconds (*outlook-mailbox-seconds*): ~
                                 what it wrote by then is listed"
                                *outlook-mailbox-seconds*)
                        (mailbox-job-notes job)))
        (:cancelled)
        (t (if (plusp (mailbox-written directory))
               (push (format nil
                             "readpst ended with an error~@[: ~a~]; what it wrote is listed"
                             (and (plusp (length said)) said))
                     (mailbox-job-notes job))
               (error "readpst could not read it~@[: ~a~]" (and (plusp (length said)) said)))))
      (mailbox-list job))))

(defun mailbox-listed-files (files)
  "Those of FILES, a folder's as MAILBOX-FOLDERS gives them, that are items of
the mailbox, each (NAME . MSG): MSG the name of the same message written as
an Outlook item beside it, or NIL. An Outlook item beside a message of the
same number is not an item of its own, and readpst's leftover
temp_file_attachment none at all."
  (let ((names (make-hash-table :test 'equal)))
    (dolist (file files) (setf (gethash (car file) names) t))
    (loop for (name) in files
          for type = (native-file-type name)
          for msg = (format nil "~a.msg" (native-file-stem name))
          unless (or (string= name "temp_file_attachment")
                     (and (string-equal type "msg") (gethash (format nil "~a.eml" (native-file-stem name)) names)))
            collect (cons name (and (string-equal type "eml") (gethash msg names) msg)))))

(defun mailbox-list (job)
  "Read each item readpst wrote in JOB's directory for the list (MAILBOX-ITEM-OF,
MAILBOX-LISTED-FILES), the folders in order, counting them as they are read,
each message's text for a search of the list written into JOB's text directory;
JOB's FOLDERS then hold them, its THREADS the tables of their links
\(MAILBOX-THREAD-TABLES), and its LABELS the labels kept for the mailbox when
its sum was made (MAILBOX-READ-LABELS)."
  (let* ((directory (mailbox-job-directory job))
         (folders (multiple-value-bind (written more) (mailbox-folders directory :limit *outlook-mailbox-most-files*)
                    (when more
                      (push (format nil
                                    "Only the first ~:d files readpst wrote are listed (*outlook-mailbox-most-files*)"
                                    *outlook-mailbox-most-files*)
                            (mailbox-job-notes job)))
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
                  collect (cons (string-right-trim "/" relative)
                                (loop for (name . msg) in files collect (item-of relative name msg))))
            (mailbox-job-threads job) (mailbox-thread-tables (mailbox-job-folders job))
            (mailbox-job-labels-name job) (and (mailbox-job-sum job) (mailbox-labels-name (mailbox-job-sum job)))
            (mailbox-job-labels job) (and (mailbox-job-labels-name job) (mailbox-read-labels (mailbox-job-labels-name job)))))
    (mailbox-number-identities (mailbox-job-folders job))))

(defparameter *mailbox-columns*
  '((" " :width 1) ("Sent" :width 16) ("From" :width 30) ("Att" :width 3 :right t) ("Subject"))
  "The columns of a mailbox's list, as TABLE-HEADING-PIECES takes them: the
mark (>), when it was sent, whom from, its attachments and its subject.")

(defparameter *mailbox-labels-column* '("Labels" :width 20)
  "The column of a mailbox's list showing each message's labels, before its
subject, while any message of the mailbox carries one.")

(defun mailbox-folder-title (folder)
  "What a mailbox's list calls FOLDER, the path readpst wrote it at."
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
                                    "Outlook mailbox ~a, ~a: ~:d item~:p in ~:d folder~:p, read by readpst~
                                     ~:[~;, deleted items included~]~%"
                                    (native-file-name (mailbox-job-path job))
                                    (octets-words (or (ignore-errors (stamp-size (path-stamp (mailbox-job-path job))))
                                                      0))
                                    count (length folders) *outlook-include-deleted*)
                            :heading)
                      (format nil
                              "~a~%"
                              (substitute-command-keys
                               (format nil
                                       "\\<mailbox-mode>\\`RET' or \\`l' opens the item on the line, a folder's ~
                                        line folds; \\<mail-view-mode>\\[mail-view-pages] in a message shows it as ~
                                        it looks, \\[quit-window] comes back; ~
                                        \\<mailbox-mode>\\[regex-search-forward] narrows the list to the items ~
                                        whose headers or contents match; \\[list-mark-entry] marks, ~
                                        \\[mailbox-export] copies out.")))))
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
                      collect (cons (format nil "~a~%" note) :warning))))
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
readpst writes, and the text searched in each, are kept for you alone in a
directory of its own in mailboxes/ in the cache directory, and deleted when it is
closed. Its searches narrow it once it is listed (MAILBOX-RENDER)."
  (let ((name (native-file-name native)))
    (reading-buffer '*mailboxes* (lambda (b) (buffer-local b 'mailbox-path)) native
                    (format nil "~a (mailbox)" name)
                    (format nil "Reading ~a with readpst, shut in the sandbox...~%" name)
                    'mailbox-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'mailbox-path) native
                            (list-preview-p buffer) *outlook-preview*)
                      ;; readpst's messages in one directory, the text searched in each in another
                      (let ((messages (path-join (buffer-private-directory buffer) "mail/"))
                            (texts (path-join (buffer-private-directory buffer) "text/")))
                        (ensure-directories messages)
                        (ensure-directories texts)
                        (start-job-thread (make-mailbox-job buffer native messages texts)
                                          #'mailbox-work
                                          :name "tuatara mailbox")))
                    :private-directory "mailboxes/")))

(defun mailbox-visit (path show)
  "Show an Outlook mailbox visited at PATH (a .pst or .ost, by its contents)
as the list of what it holds (MAILBOX-BUFFER), with SHOW, a function of the
buffer; without readpst or the sandbox, leave it to be opened as any file,
saying how to get them. Returns the buffer, or NIL for a file of another kind."
  (let ((native (file-view-path path)))
    (when (and native (eq :pst (nth-value 1 (file-kind native)))
               (file-view-can-draw-p native (mailbox-readpst)
                                     (format nil
                                             "readpst reads Outlook mailboxes, and is not installed ~
                                              (Fedora: sudo dnf install libpst; ~a)"
                                             *outlook-readpst-program*)))
      (file-view-show native (remove-if-not #'live-buffer-p *mailboxes*) (lambda (b) (buffer-local b 'mailbox-path))
                      (lambda () (mailbox-buffer native)) show))))

(define-command mailbox-back-to-directory ()
  "Close the list of the mailbox, after asking, and list its directory with rimu,
point on the mailbox: the messages readpst wrote for it are deleted, as
when it is closed any other way."
  (let* ((buffer (current-buffer))
         (path (buffer-local buffer 'mailbox-path)))
    (when (and path (y-or-n-p* (format nil "Close ~a and go back to its directory in rimu?" (buffer-name buffer))))
      (rimu-show-file path)
      (kill-buffer buffer))))

(defmethod list-entry-noun ((mode mailbox-mode))
  "A message of a mailbox, as the list's words for what is marked and acted
on call it: 3 messages marked."
  "message")

(defmethod visit-entry ((mode mailbox-mode) list item other)
  "Open ITEM of the mailbox listed in LIST: a message as one, read only, the
matches of the filters narrowing the list lit in it, and at its end the
attachments of the earlier messages of its conversation the mailbox holds
\(MAILBOX-EARLIER-ITEMS); a contact or an appointment as its text, safely;
with OTHER in another window, the list staying where it is."
  (let ((show (if other (lambda (buffer) (pop-to-buffer buffer :other t)) (lambda (buffer) (show-buffer buffer))))
        (job (buffer-local list 'mailbox-job)))
    (if (mailbox-item-message item)
        (let ((view (mail-view-buffer (mailbox-item-path item)
                                      :safe t :name (mailbox-item-subject item)
                                      :earlier (and job (mailbox-job-threads job)
                                                    (loop for (earlier . how)
                                                            in (mailbox-earlier-items (mailbox-job-threads job) item)
                                                          collect (cons (mailbox-item-path earlier)
                                                                        (mailbox-earlier-words earlier how)))))))
          (funcall show view)
          (light-search-filters list view))
        (visit-path (mailbox-item-path item) :safe t :show show))))

(defmethod preview-entry ((mode mailbox-mode) list item)
  "ITEM of the mailbox listed in LIST, beside the list: a message read in the
background into a buffer of its own for the preview, as a message is shown,
nothing in it run and nothing it points to fetched, the matches of the
list's filters lit in it; a contact or an appointment said to be one."
  (if (mailbox-item-message item)
      (let ((view (mail-view-buffer (mailbox-item-path item) :safe t :name (mailbox-item-subject item) :preview t)))
        (light-search-filters list view)
        (values view t))
      (values (side-preview-note (native-file-name (mailbox-item-path item))
                                 (substitute-command-keys
                                  "A contact or an appointment: \\<mailbox-mode>\\[list-visit] opens it as its text."))
              t)))

(defmethod entry-preview-width ((mode mailbox-mode) list)
  "The columns of a mailbox's list, a subject's first words among them."
  (declare (ignore list))
  80)

;;; Copied out

(defun mailbox-export-name (item type)
  "The name a copy of ITEM, a message, takes copied out as TYPE (\"eml\" or
\"msg\"): when it was sent and its subject, a / or \\ in it made -, as a
file's name may hold them (ATTACHMENT-FILE-NAME), else the number readpst
gave it, cut to the 255 octets a name may take (FIT-NAME); any other item
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

(defstruct (mailbox-export-job (:include job)
                               (:constructor make-mailbox-export-job (buffer items directory type &optional name))
                               (:copier nil))
  "ITEMS of the mailbox listed in BUFFER being copied out into DIRECTORY as
TYPE (\"eml\" or \"msg\", a file each, or \"mbox\", all in one file
called NAME), a job: COPIED of them so far, and NOT-COPIED, those that could
not be, each (ITEM . WHY). WRITTEN is the mbox file once made."
  items directory type name (copied 0) (not-copied '()) (written nil))

(setf (documentation 'mailbox-export-job-p 'function) "True when OBJECT is a MAILBOX-EXPORT-JOB.")

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

(defun mbox-from-line (part sent)
  "The line an mbox file writes before a message, PART (MIME-READ) sent at
SENT, a universal time or NIL: From, the address it came back to or came
from (MAILER-DAEMON for none), and the date in universal time as asctime
writes it, then a line feed."
  (let* ((header (or (mime-header part "return-path") (mime-header part "from") ""))
         (address (or (cl-ppcre:scan-to-strings "[^\\s<>\"]+@[^\\s<>\"]+" header) "MAILER-DAEMON")))
    (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time (or sent 0) 0)
      (format nil "From ~a ~a ~a ~2d ~2,'0d:~2,'0d:~2,'0d ~d~%"
              address (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") weekday)
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
message as readpst wrote it after its From line (MBOX-FROM-LINE,
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
                        (write-octets fd (string-octets (mbox-from-line (mime-read octets :end (min (length octets) 65536))
                                                                        (mailbox-item-sent item)))
                                      :path path)
                        (mbox-write-message fd path octets)
                        (incf (mailbox-export-job-copied job))))))
              (setf done t))
         (unless done
           (ignore-errors (remove-file path))
           (setf (mailbox-export-job-written job) nil)))))))

(defun mailbox-export-work (job)
  "Copy each of JOB's items out into its directory, a new file each
\(MAILBOX-EXPORT-NAME, COPY-TO-FREE-NAME), as JOB's type: an Outlook item
from the one readpst wrote beside the message; noting those that cannot be.
For the type mbox, collect them into one file (MAILBOX-EXPORT-MBOX-WORK)."
  (when (string= (mailbox-export-job-type job) "mbox")
    (return-from mailbox-export-work (mailbox-export-mbox-work job)))
  (let ((directory (mailbox-export-job-directory job)) (type (mailbox-export-job-type job)))
    (dolist (item (mailbox-export-job-items job))
      (job-check-stop)
      (let ((from (if (and (string= type "msg") (mailbox-item-message item))
                      (mailbox-item-msg item)
                      (mailbox-item-path item))))
        (handler-case
            (cond ((null from) (error "readpst wrote no Outlook item of it"))
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
  "Ask with PROMPT for a directory, made when it is not there: its path, or
NIL, said, when none is given or it cannot be made."
  (let ((directory (or (read-native-path prompt) (progn (message "No directory given: nothing copied") nil))))
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

(define-command mailbox-export-eml ()
  "Copy the messages marked in the mailbox's list
\(\\<mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, out into a directory asked for, as saved mail (.eml). A directory
not there yet is made. Each is a new file named by when it was sent and its
subject, (2) and so on added where that name is taken; nothing there is
overwritten. They are copied in the background, as readpst wrote them; the
mailbox itself is not touched. A contact or an appointment is copied as it is."
  (mailbox-export-as "eml"))

(define-command mailbox-export-msg ()
  "Copy the messages marked in the mailbox's list
\(\\<mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, out into a directory asked for, as Outlook items (.msg), as
\\[mailbox-export-eml] copies them as saved mail."
  (mailbox-export-as "msg"))

(define-command mailbox-export-mbox ()
  "Collect the messages marked in the mailbox's list
\(\\<mailbox-mode>\\[list-mark-entry]), or with none marked the one on the
current line, into one mbox file asked for, the format most mail programs
read: each message as readpst wrote it, after a From line, a line of it
beginning with From (after any number of >) written with one > more, as
mboxrd does. A name taken gets (2) and so on; nothing is overwritten. It is
written in the background; the mailbox itself is not touched. Contacts and
appointments are left out."
  (let* ((list (current-buffer))
         (items (mailbox-export-items list)))
    (if (null items)
        (message "No message on this line")
        (let* ((words (list-question nil "message" items :name-of #'mailbox-item-name))
               (path (read-native-path (format nil "Collect ~a into the mbox file: " words)))
               (name (and path (native-file-name path))))
          (cond ((or (null path) (zerop (length name))) (message "No file named: nothing collected"))
                ((not (eq :directory (path-kind (parent-path path))))
                 (message "~a is not a directory: nothing collected" (parent-path path)))
                (t (start-job-thread (make-mailbox-export-job list items (parent-path path) "mbox" name)
                                     #'mailbox-export-work :name "tuatara mailbox export")
                   (message "Collecting ~a into ~a..." words path)))))))

(define-menu mailbox-export (:title "Copy out")
  "Open the menu of copying the messages marked in the mailbox's list, or with
none marked the one on the current line, out: \\`e' as saved mail (.eml) and
\\`m' as Outlook items (.msg) into a directory, \\`b' into one mbox file,
the directory or the file asked for then. \\`a' switches to every message
the list's filters leave, marked or not. \\`C-h' there lists every action."
  ("Which"
   ("a" "All shown, not only those marked" :switch "--shown"))
  ("Copy out as"
   ("e" "Saved mail (.eml)" mailbox-export-eml)
   ("m" "Outlook items (.msg)" mailbox-export-msg)
   ("b" "One mailbox file (.mbox)" mailbox-export-mbox)))
