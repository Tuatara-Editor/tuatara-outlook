;;;; labels.lisp --- Labels put on the messages of a mailbox, kept between sessions in Tuatara's state directory.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A message of a mailbox may carry labels of your own: words put on the
;;;; messages marked, or the one on the line, and taken off again, shown on
;;;; each message's line of the list. They are kept for you alone in the state
;;;; directory, never in the mailbox nor beside it, in a file named by the
;;;; mailbox's SHA-256 sum: a copy of the mailbox, wherever it is, has the
;;;; same labels, and a mailbox changed in any way has none. Each message is
;;;; known by its folder and its Message-ID, or where it has none by the
;;;; SHA-256 sum of its headers as readpst wrote them, which stay the same
;;;; each time the mailbox is read; the numbers readpst names its files by do
;;;; not, as they follow the order of the folder and whether deleted items are
;;;; read. The labels file is written whole, in the background, after each
;;;; change.

(in-package #:outlook)

(defun mailbox-label-directory ()
  "The directory of the labels of mailboxes, for you alone, in the state directory."
  (private-state-directory "outlook/labels/"))

(defun mailbox-clean-label (text)
  "TEXT, a label as typed, as it is kept: on one line, a tab or a comma made a
space, the spaces around it taken off; NIL when nothing is left."
  (let ((label (string-trim " " (substitute-if #\Space (lambda (c) (find c '(#\Tab #\,))) (one-line text)))))
    (and (plusp (length label)) label)))

(defun mailbox-message-identity (part folder)
  "What a message of a mailbox, PART (MIME-READ) in FOLDER, is known by from one
reading of the mailbox to the next: its folder, its Message-ID (empty when it
has none) and the SHA-256 sum of its headers as readpst wrote them, apart by
tabs, a tab within either made a space. Two messages of one folder alike in
all three are told apart by their order (MAILBOX-NUMBER-IDENTITIES)."
  (let ((id (mime-header part "message-id")))
    (format nil "~a~c~a~c~a" (substitute #\Space #\Tab (one-line folder)) #\Tab
            (if id (substitute #\Space #\Tab (one-line (string-trim " " id))) "") #\Tab
            (octets-sha256 (mime-part-octets part) :start (mime-part-start part) :end (mime-part-header-end part)))))

(defun mailbox-number-identities (folders)
  "Give each message of FOLDERS, each (NAME . ITEMS), whose identity an earlier
message of the mailbox has already (a Message-ID forged or copied, headers
alike) a number after it, 2 for the second, 3 for the third, in the order
readpst wrote them, so that no two share labels."
  (let ((seen (make-hash-table :test 'equal)))
    (loop for (nil . items) in folders
          do (dolist (item items)
               (let ((identity (mailbox-item-identity item)))
                 (when identity
                   (let ((count (incf (gethash identity seen 0))))
                     (when (> count 1)
                       (setf (mailbox-item-identity item) (format nil "~a~c~d" identity #\Tab count))))))))))

(defun mailbox-journal-lines (path)
  "The lines of the journal at PATH, a labels file or the names file, in the
order they were written; none when there is no such file."
  (let ((text (file-text path :missing nil)))
    (and text (remove "" (uiop:split-string text :separator (string #\Newline)) :test #'string=))))

(defun mailbox-replay-labels (lines)
  "The labels LINES of a mailbox's journal leave, a table from each message's
identity to its labels in the order put on: each line + or -, a tab, a label,
a tab and the identity, putting the label on or taking it off. A line that
makes no sense is passed over."
  (let ((table (make-hash-table :test 'equal :synchronized t)))
    (dolist (line lines table)
      (let* ((first (position #\Tab line))
             (second (and first (position #\Tab line :start (1+ first))))
             (label (and second (mailbox-clean-label (subseq line (1+ first) second))))
             (identity (and second (subseq line (1+ second)))))
        (when (and label (plusp (length identity)))
          (let ((had (gethash identity table)))
            (cond ((string= "+" line :end2 first)
                   (setf (gethash identity table) (append (remove label had :test #'string=) (list label))))
                  ((string= "-" line :end2 first)
                   (let ((left (remove label had :test #'string=)))
                     (if left (setf (gethash identity table) left) (remhash identity table)))))))))))

(defun mailbox-label-line (sign label identity)
  "The journal's line putting LABEL on the message IDENTITY, SIGN +, or
taking it off, SIGN -."
  (format nil "~a~c~a~c~a~%" sign #\Tab label #\Tab identity))

(defparameter *mailbox-journal-slack* 256
  "How many lines a labels journal may hold beyond twice the labels it leaves
before it is written again with those alone.")

(defun mailbox-compact-labels (path)
  "Write the labels journal at PATH again with only the lines putting on the
labels it leaves, holding its lock so that no session's line is lost
meanwhile; when it holds more than twice as many lines as that, and
*MAILBOX-JOURNAL-SLACK* more."
  (call-with-state-lock
   path
   (lambda ()
     (let* ((lines (mailbox-journal-lines path))
            (table (mailbox-replay-labels lines))
            (live (loop for labels being the hash-values of table sum (length labels))))
       (when (> (length lines) (+ (* 2 live) *mailbox-journal-slack*))
         (replace-file path
                       (with-output-to-string (out)
                         (maphash (lambda (identity labels)
                                    (dolist (label labels) (write-string (mailbox-label-line "+" label identity) out)))
                                  table))
                       :mode #o600))))
   :wait 5))

(defun mailbox-labels-name (sum)
  "The name of the labels journal of the mailbox whose SHA-256 sum is SUM, as
it is read now: SUM, or SUM.with-deleted while *OUTLOOK-INCLUDE-DELETED* has
readpst list deleted items too. The two are kept apart because readpst numbers
a folder's items in its own order, deleted ones among them when they are read,
so that the order telling alike messages apart (MAILBOX-NUMBER-IDENTITIES)
changes with the setting."
  (format nil "~a~:[~;.with-deleted~]" sum *outlook-include-deleted*))

(defun mailbox-read-labels (name)
  "The labels kept in the labels journal NAME (MAILBOX-LABELS-NAME), a table from each
message's identity (MAILBOX-MESSAGE-IDENTITY) to its labels; empty when none
are kept. They are a journal, every change a line added at its end, read from
the first (MAILBOX-REPLAY-LABELS), and written again shorter when it has
grown long (MAILBOX-COMPACT-LABELS)."
  (let ((path (path-join (mailbox-label-directory) name)))
    (when (> (length (mailbox-journal-lines path)) *mailbox-journal-slack*)
      (mailbox-compact-labels path))
    (mailbox-replay-labels (mailbox-journal-lines path))))

(defun mailbox-label-names ()
  "Every label put on a message of any mailbox, as the names journal of the
labels' directory keeps them, a line each, + and a tab before it."
  (remove-duplicates (loop for line in (mailbox-journal-lines (path-join (mailbox-label-directory) "names"))
                           for name = (and (uiop:string-prefix-p (format nil "+~c" #\Tab) line)
                                           (mailbox-clean-label (subseq line 2)))
                           when name collect name)
                     :test #'string= :from-end t))

(defstruct (mailbox-labels-job (:include job)
                               (:constructor make-mailbox-labels-job (buffer journal lines names))
                               (:copier nil))
  "Changes to the labels of a mailbox being kept, a job: LINES, added to the
end of the labels journal named JOURNAL (MAILBOX-LABELS-NAME), and NAMES, the
lines of the labels not used before, added to the names journal."
  journal lines names)

(setf (documentation 'mailbox-labels-job-p 'function) "True when OBJECT is a MAILBOX-LABELS-JOB.")

(define-job-words mailbox-labels-job
  :doing "Keeping the labels"
  :description "Keeping the labels of a mailbox's messages"
  :verb "keep the labels"
  :left-words "not kept")

(defun mailbox-append-lines (path lines)
  "Add LINES to the end of the journal at PATH in one write, a line end put
first when its last line has none, as one cut short by a crash while it was
written leaves it, so that the first of LINES is not joined to it. Called
holding the journal's lock."
  (let ((text (file-text path :missing "")))
    (append-to-file path (format nil "~:[~;~%~]~{~a~}"
                                 (and (plusp (length text)) (char/= #\Newline (char text (1- (length text)))))
                                 lines))))

(defun mailbox-labels-work (job)
  "Add JOB's lines to the end of the mailbox's labels journal, and its names
to the names journal, each in one write while holding the journal's lock, so
that what other sessions add meanwhile is kept and a journal written again
shorter (MAILBOX-COMPACT-LABELS) loses nothing; an error when the lock is not
had in a few seconds."
  (let ((directory (mailbox-label-directory)))
    (flet ((add (name lines)
             (when lines
               (let ((path (path-join directory name)))
                 (unless (call-with-state-lock path (lambda () (mailbox-append-lines path lines) t) :wait 5)
                   (error "~a is held by another session" path))))))
      (add (mailbox-labels-job-journal job) (mailbox-labels-job-lines job))
      (add "names" (mailbox-labels-job-names job)))))

(defun mailbox-keep-labels (job lines names)
  "Have LINES, changes to the labels of JOB, a mailbox read, and NAMES, lines
of labels not used before, added to their journals in the background; once
the writing under way has ended when there is one (MAILBOX-LABELS-JOB's
JOB-TAKE-IN starts it then)."
  (setf (mailbox-job-labels-pending job)
        (cons (append (car (mailbox-job-labels-pending job)) lines)
              (append (cdr (mailbox-job-labels-pending job)) names)))
  (let ((writer (mailbox-job-labels-writer job)) (pending (mailbox-job-labels-pending job)))
    (unless (or (and writer (job-running-p writer)) (and (null (car pending)) (null (cdr pending))))
      (setf (mailbox-job-labels-pending job) nil
            (mailbox-job-labels-writer job)
            (start-job-thread (make-mailbox-labels-job (job-buffer job) (mailbox-job-labels-name job) (car pending)
                                                       (cdr pending))
                              #'mailbox-labels-work :name "tuatara mailbox labels")))))

(defmethod job-take-in ((job mailbox-labels-job))
  "Say why the labels could not be kept, when they could not; write the
changes made meanwhile."
  (unless (eq (job-status job) :done)
    (message "The labels were not kept: ~a" (ignore-errors (job-ending-words job))))
  (let ((list (job-buffer job)))
    (when (live-buffer-p list)
      (let ((mailbox (buffer-local list 'mailbox-job)))
        (when mailbox
          (mailbox-keep-labels mailbox nil nil))))))

(defun mailbox-item-labels (job item)
  "The labels on ITEM of the mailbox JOB read, a list."
  (let ((identity (mailbox-item-identity item)))
    (and identity (mailbox-job-labels job) (values (gethash identity (mailbox-job-labels job))))))

(defun mailbox-labels-used (job)
  "Every label on a message of the mailbox JOB read, each once, in order."
  (let ((seen '()))
    (maphash (lambda (identity labels) (declare (ignore identity)) (dolist (l labels) (pushnew l seen :test #'string=)))
             (mailbox-job-labels job))
    (sort seen #'string<)))

(defun mailbox-labelled-items ()
  "The mailbox read for the list here and the messages a label command acts
on: those marked, or the one on the line; as two values. NIL, said, when
there are none, or when the mailbox's labels cannot be kept."
  (let* ((list (current-buffer))
         (job (buffer-local list 'mailbox-job))
         (items (remove-if-not (lambda (item) (and (mailbox-item-p item) (mailbox-item-identity item)))
                               (list-acted-on list))))
    (cond ((null job) (message "The mailbox is not listed yet") nil)
          ((null (mailbox-job-labels job))
           (message "No labels can be kept for this mailbox: its SHA-256 sum could not be made") nil)
          ((null items) (message "No message on this line") nil)
          (t (values job items)))))

(defun mailbox-relabel (job items change)
  "Give each of ITEMS of the mailbox JOB read the labels CHANGE, a function of
its labels, makes of them; keep the changes, show the list again and work its
filters out again, so that a filter on labels leaves what it now matches."
  (let ((table (mailbox-job-labels job)) (lines '()) (known (mailbox-label-names)) (names '()))
    (dolist (item items)
      (let* ((identity (mailbox-item-identity item))
             (had (gethash identity table))
             (labels (funcall change had)))
        (dolist (label labels)
          (unless (member label had :test #'string=)
            (push (mailbox-label-line "+" label identity) lines)
            (unless (or (member label known :test #'string=) (member label names :test #'string=))
              (push label names))))
        (dolist (label had)
          (unless (member label labels :test #'string=)
            (push (mailbox-label-line "-" label identity) lines)))
        (if labels (setf (gethash identity table) labels) (remhash identity table))))
    (mailbox-keep-labels job (nreverse lines) (mapcar (lambda (name) (format nil "+~c~a~%" #\Tab name)) (nreverse names)))
    (let ((list (job-buffer job)))
      (mailbox-render list job :keep)
      (refilter-search-filters list))))

(define-command mailbox-label ()
  "Put labels on the messages marked in the mailbox's list
\(\\<mailbox-mode>\\[list-mark-entry]), or with none marked on the one on the
current line: several, apart by commas, the labels used before offered and
new ones typed. Each message's labels show on its line. They are kept for you
alone in Tuatara's state directory, by the mailbox's SHA-256 sum, never in
the mailbox nor beside it, and come back whenever that mailbox is opened again.
\\[mailbox-unlabel] takes them off and \\[mailbox-filter-labels] shows only
the messages carrying them."
  (multiple-value-bind (job items) (mailbox-labelled-items)
    (when job
      (let ((labels (remove nil (mapcar #'mailbox-clean-label
                                        (read-several (format nil "Label ~a with: "
                                                              (list-question nil "message" items :name-of #'mailbox-item-name))
                                                      (lambda () (union (mailbox-labels-used job) (mailbox-label-names)
                                                                        :test #'string=))
                                                      :none "no label" :more "no more labels")))))
        (if (null labels)
            (message "No label given: nothing labelled")
            (progn (mailbox-relabel job items (lambda (had) (remove-duplicates (append had labels) :test #'string=
                                                                                                    :from-end t)))
                   (message "~{~a~^, ~} put on ~a" labels (list-question nil "message" items
                                                                        :name-of #'mailbox-item-name))))))))

(define-command mailbox-unlabel ()
  "Take labels off the messages marked in the mailbox's list, or with none
marked off the one on the current line: several, apart by commas, chosen from
those they carry (\\<mailbox-mode>\\[mailbox-label])."
  (multiple-value-bind (job items) (mailbox-labelled-items)
    (when job
      (let ((carried (sort (remove-duplicates (loop for item in items append (mailbox-item-labels job item))
                                              :test #'string=)
                           #'string<)))
        (if (null carried)
            (message "~a no label" (if (rest items) "They carry" "It carries"))
            (let ((labels (read-several (format nil "Take off ~a: "
                                                (list-question nil "message" items :name-of #'mailbox-item-name))
                                        (lambda () carried) :none "no label" :more "no more labels")))
              (if (null labels)
                  (message "No label given: nothing taken off")
                  (progn (mailbox-relabel job items (lambda (had)
                                                      (remove-if (lambda (l) (member l labels :test #'string=)) had)))
                         (message "~{~a~^, ~} taken off" labels)))))))))

(defun mailbox-labels-pattern (labels)
  "A regular expression matching a line that is one of LABELS whole, as a
filter on the labels of a message tries it, each label's special characters
escaped (PATTERN-REGEX)."
  (format nil "^(?:~{~a~^|~})$" (mapcar (lambda (label) (pattern-regex label nil)) labels)))

(define-command mailbox-filter-labels ()
  "Narrow the mailbox's list to the messages carrying a label, or any of
several, chosen apart by commas from those its messages carry
\(\\<mailbox-mode>\\[mailbox-label]); with \\[universal-argument], to those
carrying none of them. It is one of the list's filters, kept with the others
and narrowing what they leave: \\[search-filter-list] lists them and
\\[search-filter-remove-all] takes them all away. \\[search-filter-show-records]
with the field \"your label\" does the same with a pattern of your own.
Labels are told apart by case: kea is not Kea. A message labelled or
unlabelled after is shown or hidden as the filters now say."
  (let* ((list (current-buffer))
         (job (buffer-local list 'mailbox-job))
         (used (and job (mailbox-job-labels job) (mailbox-labels-used job)))
         (hide (and (prefix-argument) t)))
    (if (null used)
        (message "No message of this mailbox carries a label: \\<mailbox-mode>\\[mailbox-label] puts them on")
        (let ((labels (read-several (format nil "Show the messages carrying ~:[~;none of ~]the labels: " hide)
                                    (lambda () used) :none "no label" :more "no more labels")))
          (if (null labels)
              (message "No label given: the list is as it was")
              (add-search-filter list (mailbox-labels-pattern labels) :part "your label" :hide hide
                                                                       :ignore-case nil))))))
