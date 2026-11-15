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
;;;; read. Each change is a line added to the file in the
;;;; background. A line beginning @ tells what the file is for, so that the
;;;; label manager can show it: the mailbox's file name, where it was last
;;;; opened, its size, its sum and when its labels last changed; the latest
;;;; such line holds, and a file with none still reads.

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
  "The lines of the journal at PATH, a labels file, in the
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

(defun mailbox-meta-text (meta)
  "The journal's line for META, a list of a mailbox's file name, path, size,
SHA-256 sum and when its labels last changed (a universal time): @, a tab,
mailbox, and the five apart by tabs, a tab within a name or path made a space."
  (destructuring-bind (name path size sum changed) meta
    (format nil "@~cmailbox~c~a~c~a~c~d~c~a~c~d~%" #\Tab #\Tab (substitute #\Space #\Tab (one-line name)) #\Tab
            (substitute #\Space #\Tab (one-line path)) #\Tab size #\Tab sum #\Tab changed)))

(defun mailbox-meta-line (job &optional changed)
  "The journal's line telling which mailbox the labels of JOB, a mailbox read,
are of (MAILBOX-META-TEXT): its file's name, its path as it was opened, its
size, its sum and when its labels last changed, CHANGED, now when not given."
  (let ((path (mailbox-job-path job)))
    (mailbox-meta-text (list (native-file-name path) (native path)
                             (or (ignore-errors (stamp-size (path-stamp path))) 0)
                             (mailbox-job-sum job) (or changed (get-universal-time))))))

(defun mailbox-journal-meta (lines)
  "What the latest @ line of the journal LINES tells (MAILBOX-META-LINE): a
list of the file's name, its path, its size, its sum and when its labels
changed; NIL for a journal with none."
  (let ((found nil))
    (dolist (line lines found)
      (when (uiop:string-prefix-p (format nil "@~cmailbox~c" #\Tab #\Tab) line)
        (let ((fields (uiop:split-string line :separator '(#\Tab))))
          (when (= (length fields) 7)
            (let ((size (parse-integer (fifth fields) :junk-allowed t))
                  (changed (parse-integer (seventh fields) :junk-allowed t)))
              (when (and size changed)
                (setf found (list (third fields) (fourth fields) size (sixth fields) changed))))))))))

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
                         (let ((meta (mailbox-journal-meta lines)))
                           (when meta
                             (write-string (mailbox-meta-text meta) out)))
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
    (let ((lines (mailbox-journal-lines path)))
      (values (mailbox-replay-labels lines) (mailbox-journal-meta lines) (and lines t)))))

(defun mailbox-note-mailbox (job meta)
  "Add a line to the labels journal of JOB, a mailbox just read, telling what
mailbox it is (MAILBOX-META-LINE) when META, what the journal told before
(MAILBOX-JOURNAL-META), is not what it now is: another name, path, size or
sum, or none. When its labels last changed stays as it was. Nothing is
written for a mailbox that has no journal; a lock not had in a few seconds
leaves it for the next time."
  (let* ((now (mailbox-journal-meta (list (mailbox-meta-line job 0))))
         (path (path-join (mailbox-label-directory) (mailbox-job-labels-name job))))
    (unless (equal (subseq now 0 4) (and meta (subseq meta 0 4)))
      (call-with-state-lock
       path (lambda ()
              (mailbox-append-lines path (list (mailbox-meta-line job (or (fifth meta)
                                                                          (ignore-errors (file-write-date path))
                                                                          0)))))
       :wait 5))))

(defstruct (mailbox-labels-job (:include job)
                               (:constructor make-mailbox-labels-job (buffer journal lines))
                               (:copier nil))
  "Changes to the labels of a mailbox being kept, a job: LINES, added to the
end of the labels journal named JOURNAL (MAILBOX-LABELS-NAME)."
  journal lines)

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
  "Add JOB's lines to the end of the mailbox's labels journal in one write
while holding the journal's lock, so that what other sessions add meanwhile is
kept and a journal written again shorter (MAILBOX-COMPACT-LABELS) loses
nothing; an error when the lock is not had in a few seconds."
  (when (mailbox-labels-job-lines job)
    (let ((path (path-join (mailbox-label-directory) (mailbox-labels-job-journal job))))
      (unless (call-with-state-lock path (lambda () (mailbox-append-lines path (mailbox-labels-job-lines job)) t)
                                    :wait 5)
        (error "~a is held by another session" path)))))

(defun mailbox-keep-labels (job lines)
  "Have LINES, changes to the labels of JOB, a mailbox read, added to its
journal in the background; once the writing under way has ended when there is
one (MAILBOX-LABELS-JOB's JOB-TAKE-IN starts it then)."
  (setf (mailbox-job-labels-pending job)
        (append (mailbox-job-labels-pending job) lines (and lines (list (mailbox-meta-line job)))))
  (let ((writer (mailbox-job-labels-writer job)) (pending (mailbox-job-labels-pending job)))
    (unless (or (and writer (job-running-p writer)) (null pending))
      (setf (mailbox-job-labels-pending job) nil
            (mailbox-job-labels-writer job)
            (start-job-thread (make-mailbox-labels-job (job-buffer job) (mailbox-job-labels-name job) pending)
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
          (mailbox-keep-labels mailbox nil))))))

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

(defvar *mailbox-labels-forgetting* '()
  "The journals being forgotten by the label manager (OUTLOOK-LABELS-FORGET)
that a job in the background has not finished with: no label is put on their
mailboxes meanwhile, as it would make a journal again.")

(defun mailbox-labelled-items ()
  "The mailbox read for the list here and the messages a label command acts
on: those marked, or the one on the line; as two values. NIL, said, when
there are none, or when the mailbox's labels cannot be kept."
  (let* ((list (current-buffer))
         (job (buffer-local list 'mailbox-job))
         (items (remove-if-not (lambda (item) (and (mailbox-item-p item) (mailbox-item-identity item)))
                               (list-acted-on list))))
    (cond ((null job) (message "The mailbox is not listed yet") nil)
          ((member (mailbox-job-labels-name job) *mailbox-labels-forgetting* :test #'equal)
           (message "The labels of this mailbox are being forgotten: wait a moment") nil)
          ((null (mailbox-job-labels job))
           (message "No labels can be kept for this mailbox: its SHA-256 sum could not be made") nil)
          ((null items) (message "No message on this line") nil)
          (t (values job items)))))

(defun mailbox-relabel (job items change)
  "Give each of ITEMS of the mailbox JOB read the labels CHANGE, a function of
its labels, makes of them; keep the changes, show the list again and work its
filters out again, so that a filter on labels leaves what it now matches."
  (let ((table (mailbox-job-labels job)) (lines '()))
    (dolist (item items)
      (let* ((identity (mailbox-item-identity item))
             (had (gethash identity table))
             (labels (funcall change had)))
        (dolist (label labels)
          (unless (member label had :test #'string=)
            (push (mailbox-label-line "+" label identity) lines)))
        (dolist (label had)
          (unless (member label labels :test #'string=)
            (push (mailbox-label-line "-" label identity) lines)))
        (if labels (setf (gethash identity table) labels) (remhash identity table))))
    (mailbox-keep-labels job (nreverse lines))
    (let ((list (job-buffer job)))
      (mailbox-render list job :keep)
      (refilter-search-filters list))))

(define-command outlook-label ()
  "Put labels on the messages marked in the mailbox's list
\(\\<outlook-mailbox-mode>\\[list-mark-entry]), or with none marked on the one on the
current line: several, apart by commas, the labels used before offered and
new ones typed. Each message's labels show on its line. They are kept for you
alone in Tuatara's state directory, by the mailbox's SHA-256 sum, never in
the mailbox nor beside it, and come back whenever that mailbox is opened again.
\\[outlook-unlabel] takes them off and \\[outlook-filter-labels] shows only
the messages carrying them."
  (multiple-value-bind (job items) (mailbox-labelled-items)
    (when job
      (let ((labels (remove nil (mapcar #'mailbox-clean-label
                                        (read-several (format nil "Label ~a with: "
                                                              (list-question nil "message" items :name-of #'mailbox-item-name))
                                                      (lambda () (mailbox-labels-used job))
                                                      :none "no label" :more "no more labels")))))
        (if (null labels)
            (message "No label given: nothing labelled")
            (progn (mailbox-relabel job items (lambda (had) (remove-duplicates (append had labels) :test #'string=
                                                                                                    :from-end t)))
                   (message "~{~a~^, ~} put on ~a" labels (list-question nil "message" items
                                                                        :name-of #'mailbox-item-name))))))))

(define-command outlook-unlabel ()
  "Take labels off the messages marked in the mailbox's list, or with none
marked off the one on the current line: several, apart by commas, chosen from
those they carry (\\<outlook-mailbox-mode>\\[outlook-label])."
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

(define-command outlook-filter-labels ()
  "Narrow the mailbox's list to the messages carrying a label, or any of
several, chosen apart by commas from those its messages carry
\(\\<outlook-mailbox-mode>\\[outlook-label]); with \\[universal-argument], to those
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
        (message "No message of this mailbox carries a label: \\<outlook-mailbox-mode>\\[outlook-label] puts them on")
        (let ((labels (read-several (format nil "Show the messages carrying ~:[~;none of ~]the labels: " hide)
                                    (lambda () used) :none "no label" :more "no more labels")))
          (if (null labels)
              (message "No label given: the list is as it was")
              (add-search-filter list (mailbox-labels-pattern labels) :part "your label" :hide hide
                                                                       :ignore-case nil))))))
