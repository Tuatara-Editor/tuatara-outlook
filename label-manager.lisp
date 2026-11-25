;;;; label-manager.lisp --- The labels kept for mailboxes, listed to rename, delete, forget, copy out and in.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x outlook-label-manager lists every mailbox that has labels in the
;;;; state directory, one section each, and under it each label with the
;;;; number of messages carrying it. A label can be renamed or deleted from
;;;; every message of its mailbox, a mailbox's whole set forgotten, copied out
;;;; into a file of your choice and read back from one. Each change is lines
;;;; added to the mailbox's journal (labels.lisp), so that what another
;;;; session labelled meanwhile stays.

(in-package #:outlook)

(define-major-mode outlook-label-manager-mode sectioned-list-mode
  "The labels of mailboxes (\\[outlook-label-manager]): a section for each
mailbox that has labels, with its file's name, where it was last opened, whether
the file is there now, the start of its SHA-256 sum, how many labels and
messages it has and when they last changed; a mailbox read with its deleted
items has a section apart. Under it a line for each label and how many
messages carry it. \\<outlook-label-manager-mode>\\[list-visit] on a label
opens its mailbox, when its file is there, \\[outlook-label-open] on any line
of the section.
\\[list-mark-entry] marks a label, \\[list-mark-for-deletion] marks it for
deletion, \\[list-unmark-entry] and \\[list-unmark-all] unmark; \\[list-execute]
takes the labels marked for deletion off every message of their mailboxes,
after asking. \\[outlook-label-rename] renames the label on the line, or the
labels marked, in its mailbox. \\[outlook-labels-forget] forgets every label of
the mailbox on the line, after asking. \\[outlook-labels-export] writes the
labels of the mailbox on the line into a file you choose and
\\[outlook-labels-import] reads such a file back, into the mailbox whose
SHA-256 sum it holds, adding to the labels there. \\`h' folds the section of
a mailbox's heading, \\[quit-window] comes back; \\[revert-buffer] lists them
again."
  :lighter "Mailbox labels" :interactive nil)

(define-record (mailbox-label-set (:copier nil))
  "The labels kept for one mailbox as its journal leaves them. JOURNAL is the
journal's name (MAILBOX-LABELS-NAME), SUM the SHA-256 sum it is of, DELETED
true when it is the one kept while deleted items are read. META is what the
journal tells of the mailbox (MAILBOX-JOURNAL-META), LABELS each label with how
many messages carry it, (LABEL . COUNT) in order, MESSAGES how many carry any
and SIZE the octets the journal takes."
  (journal nil :documentation "The name of the journal of MAILBOX-LABEL-SET (MAILBOX-LABELS-NAME).")
  (sum nil :documentation "The SHA-256 sum of the mailbox the journal is of.")
  (deleted nil :documentation "True when MAILBOX-LABEL-SET is the one kept while deleted items are read.")
  (meta nil :documentation "What the journal tells of the mailbox (MAILBOX-JOURNAL-META).")
  (labels nil :documentation "Each label with how many messages carry it, (LABEL . COUNT) in order.")
  (messages nil :documentation "How many messages carry any label.")
  (size nil :documentation "The octets the journal takes."))

(defparameter *mailbox-with-deleted-ending* ".with-deleted"
  "What a labels journal's name ends in when it is the one kept while deleted
items are read (MAILBOX-LABELS-NAME).")

(defun mailbox-sum-p (text)
  "True when TEXT is a SHA-256 sum as the journals are named by: exactly 64
lowercase hexadecimal digits."
  (and (stringp text) (= 64 (length text)) (every (lambda (c) (digit-char-p c 16)) text)
       (string= text (string-downcase text))))

(defun mailbox-journal-sum (name)
  "The SHA-256 sum of the mailbox whose labels the journal named NAME keeps."
  (subseq name 0 (or (search *mailbox-with-deleted-ending* name) (length name))))

(defun mailbox-journal-name-p (name)
  "True when NAME, a file of the labels directory, is a mailbox's journal: a
sum (MAILBOX-SUM-P), with the ending of deleted items read or without. Lock
files, files being written again and the names file earlier versions kept are
not."
  (let ((sum (mailbox-journal-sum name)))
    (and (mailbox-sum-p sum)
         (or (string= name sum) (string= name (concatenate 'string sum *mailbox-with-deleted-ending*))))))

(defun mailbox-read-label-set (name size)
  "The labels the journal NAME of SIZE octets leaves, as a MAILBOX-LABEL-SET."
  (let* ((lines (mailbox-journal-lines (path-join (mailbox-label-directory) name)))
         (table (mailbox-replay-labels lines))
         (counts (make-hash-table :test 'equal)))
    (loop for labels being the hash-values of table
          do (dolist (label labels) (incf (gethash label counts 0))))
    (make-mailbox-label-set
     :journal name :deleted (uiop:string-suffix-p name *mailbox-with-deleted-ending*) :sum (mailbox-journal-sum name)
     :meta (mailbox-journal-meta lines) :size size :messages (hash-table-count table)
     :labels (sort (loop for label being the hash-keys of counts using (hash-value count) collect (cons label count))
                   #'string< :key #'car))))

(defun mailbox-label-sets ()
  "Every mailbox that has a journal in the labels directory, as
MAILBOX-LABEL-SETs, the latest changed first, those that do not say when after.
Reads every journal: for a job in the background."
  (sort (loop for (name kind size) in (directory-entries (mailbox-label-directory))
              when (and (eq kind :regular) (mailbox-journal-name-p name))
                collect (mailbox-read-label-set name size))
        #'> :key (lambda (set) (or (fifth (mailbox-label-set-meta set)) 0))))

(defvar *mailbox-label-sets* nil
  "The mailboxes with labels as the label manager's last job read them
\(MAILBOX-LABEL-SETS), what the manager shows without reading anything.")

(defvar *mailbox-label-files* (make-hash-table :test 'equal :synchronized t)
  "Whether each mailbox's file is there (:THERE or :GONE), by path, as looked at
in the background since the label manager was last opened.")

(define-record (mailbox-labels-manager-job (:include job)
                                       (:constructor make-mailbox-labels-manager-job
                                        (buffer words action lists &optional forgetting))
                                       (:copier nil))
  "Work for the label manager in BUFFER, in the background: ACTION, a function
of no arguments or NIL, changes labels and gives the words to say of it (an
error it signals is said, the rest still done); then the journals are read
again (SETS) and each mailbox's file looked for. LISTS are the lists of
mailboxes open as they were when the job began (MAILBOX-OPEN-LISTS), whose
labels are read again (TABLES, each (BUFFER JOURNAL TABLE IDLE WRITER)). WORDS
say what is being done, RESULT what the action said. FORGETTING is the journal
the job forgets, if it does (*MAILBOX-LABELS-FORGETTING*), let go of when the
job is taken in, however it ended."
  (words nil :documentation "What MAILBOX-LABELS-MANAGER-JOB is doing, in words.")
  (action nil
          :documentation "A function of no arguments, or NIL, that changes labels and gives the words to say of it.")
  (lists nil :documentation "The lists of mailboxes open as they were when the job began (MAILBOX-OPEN-LISTS).")
  (sets nil :documentation "The journals read again once the action is done.")
  (tables nil :documentation "The labels of the open lists read again, each (BUFFER JOURNAL TABLE IDLE WRITER).")
  (result nil :documentation "What the action said.")
  (forgetting nil
              :documentation "The journal the job lets go of when it is taken in, however it ended
\(*MAILBOX-LABELS-FORGETTING*), or NIL."))

(define-job-words mailbox-labels-manager-job
  :doing (mailbox-labels-manager-job-words job)
  :description "Reading or changing the labels kept for mailboxes"
  :verb "change the labels of mailboxes"
  :left-words "not done")

(defun mailbox-open-lists ()
  "The lists of mailboxes open in this session whose labels are kept, each
\(BUFFER JOURNAL IDLE WRITER): IDLE true when none of its changes waits to be
kept or is being kept, WRITER the job that last kept some."
  (loop for buffer in *mailboxes*
        for job = (and (live-buffer-p buffer) (buffer-local buffer 'mailbox-job))
        when (and job (mailbox-job-labels-name job))
          collect (list buffer (mailbox-job-labels-name job) (mailbox-list-idle-p job)
                        (mailbox-job-labels-writer job))))

(defun mailbox-list-idle-p (job)
  "True when no change to the labels of JOB, a mailbox read, waits to be kept or
is being kept."
  (and (null (mailbox-job-labels-pending job))
       (not (and (mailbox-job-labels-writer job) (job-running-p (mailbox-job-labels-writer job))))))

(defun mailbox-labels-manager-work (job)
  "Do JOB's action, then read the journals again, look for each mailbox's file
and read the labels of the mailboxes open again."
  (when (mailbox-labels-manager-job-action job)
    (setf (mailbox-labels-manager-job-result job)
          (handler-case (funcall (mailbox-labels-manager-job-action job))
            (error (e) (format nil "Not all were changed: ~a" (ignore-errors (failure-words e)))))))
  (let ((sets (mailbox-label-sets)))
    (setf (mailbox-labels-manager-job-sets job) sets)
    (dolist (set sets)
      (job-check-stop)
      (let ((path (second (mailbox-label-set-meta set))))
        (when (and path (plusp (length path)) (not (gethash path *mailbox-label-files*)))
          (setf (gethash path *mailbox-label-files*) (if (ignore-errors (path-stamp path)) :there :gone))))))
  (setf (mailbox-labels-manager-job-tables job)
        (loop for (buffer journal idle writer) in (mailbox-labels-manager-job-lists job)
              collect (list buffer journal (mailbox-read-labels journal) idle writer))))

(defmethod job-take-in ((job mailbox-labels-manager-job))
  "Show what the job found: the label manager again, the labels of the mailboxes
open as they are now, except those still being written, and what the action
said."
  (let ((journal (mailbox-labels-manager-job-forgetting job)))
    (when journal
      (setf *mailbox-labels-forgetting* (remove journal *mailbox-labels-forgetting* :test #'string=))))
  (if (not (eq (job-status job) :done))
      (message "The labels were not read: ~a~@[ (done: ~a)~]" (ignore-errors (job-ending-words job))
               (mailbox-labels-manager-job-result job))
      (progn
        (setf *mailbox-label-sets* (mailbox-labels-manager-job-sets job))
        (loop for (buffer journal table idle writer) in (mailbox-labels-manager-job-tables job)
              for mailbox = (and (live-buffer-p buffer) (buffer-local buffer 'mailbox-job))
              when (and mailbox idle (equal (mailbox-job-labels-name mailbox) journal)
                        (mailbox-list-idle-p mailbox) (eq writer (mailbox-job-labels-writer mailbox)))
                do (setf (mailbox-job-labels mailbox) table)
                   (mailbox-render buffer mailbox :keep)
                   (mailbox-refilter-labels buffer))
        (when (mailbox-labels-manager-job-result job) (message "~a" (mailbox-labels-manager-job-result job)))))
  (let ((list (job-buffer job)))
    (when (live-buffer-p list)
      (mailbox-labels-render list :keep))))

(defun mailbox-labels-working (list)
  "The job of the label manager LIST under way, or NIL."
  (first (remove-if-not #'mailbox-labels-manager-job-p (jobs-of list :running t))))

(defun mailbox-labels-free-p (list)
  "True when nothing is being done for the label manager LIST; otherwise say
what is, and say so while the labels are still being read for the first time."
  (let ((working (mailbox-labels-working list)))
    (when working (message "Wait: ~a" (mailbox-labels-manager-job-words working)))
    (null working)))

(defun mailbox-labels-start (list words action &key forgetting)
  "Have ACTION, a function of no arguments that changes labels and gives the
words to say of it (or NIL), done in the background for the label manager
LIST, WORDS saying what is being done in the meantime. FORGETTING, the name of
the journal the action deletes, is kept from being labelled (*MAILBOX-LABELS-
FORGETTING*) from when the job has started till it is taken in. Returns true,
or false, said, when something is being done already."
  (when (mailbox-labels-free-p list)
    (start-job-thread (make-mailbox-labels-manager-job list words action (mailbox-open-lists) forgetting)
                      #'mailbox-labels-manager-work :name "tuatara mailbox labels manager")
    (when forgetting (push forgetting *mailbox-labels-forgetting*))
    (mailbox-labels-render list :keep)
    t))

(defun mailbox-label-set-words (set)
  "What the manager's section for SET says: the file's name, where it was last
opened, whether it is there, the start of its sum, how many labels it has on
how many messages, and when they last changed."
  (destructuring-bind (&optional name path size sum changed) (mailbox-label-set-meta set)
    (declare (ignore size sum))
    (let ((here (and path (gethash path *mailbox-label-files*))))
      (format nil "~a~:[~; (with deleted items)~]  ~@[~a~]  ~a  ~a  ~d label~:p on ~:d message~:p~@[, changed ~a~]"
              (or name "a mailbox not opened since labels were kept without its name")
              (mailbox-label-set-deleted set)
              (and path (plusp (length path)) (abbreviated-path path))
              (case here (:there "there") (:gone "not there now") (t "..."))
              (subseq (mailbox-label-set-sum set) 0 (min 8 (length (mailbox-label-set-sum set))))
              (length (mailbox-label-set-labels set)) (mailbox-label-set-messages set)
              (and changed (short-time-words changed))))))

(defun mailbox-label-room (sets)
  "The room the journals of SETS, MAILBOX-LABEL-SETs, take, in words."
  (octets-words (reduce #'+ sets :key #'mailbox-label-set-size)))

(defun mailbox-labels-place-words ()
  "Where labels are kept, as the lists say it: the directory, abbreviated
\(~), and the command that manages them."
  (format nil "Labels are kept in ~a (~a)" (abbreviated-path (mailbox-label-directory))
          (substitute-command-keys "\\[outlook-label-manager]")))

(defun mailbox-labels-render (list &optional (point :first))
  "Fill LIST, the label manager, with a section for each mailbox that has
labels (*MAILBOX-LABEL-SETS*, as the last job read them), a line under it for
each label, their marks kept. A job under way is said in the heading."
  (let* ((sets *mailbox-label-sets*)
         (working (mailbox-labels-working list))
         (before (list (cons (if (and working (null sets))
                                       (format nil "Labels of mailboxes: still reading (~a...)~%"
                                               (mailbox-labels-manager-job-words working))
                                       (format nil
                                               "Labels of mailboxes: ~:d ~:[mailboxes~;mailbox~], taking ~a~
                                                ~@[ (~a...)~]~%"
                                               (length sets) (= 1 (length sets)) (mailbox-label-room sets)
                                               (and working (mailbox-labels-manager-job-words working))))
                             :heading)
                       (format nil "~a~%~%"
                               (filled-words
                                (substitute-command-keys
                                 (format nil "~a. \\<outlook-label-manager-mode>\\[list-visit] on a label opens ~
                                              its mailbox, \\[outlook-label-rename] renames a ~
                                              label, \\[list-mark-for-deletion] and \\[list-execute] delete labels, ~
                                              \\[outlook-labels-forget] forgets a mailbox's labels, ~
                                              \\[outlook-labels-export] and \\[outlook-labels-import] copy them out ~
                                              and in."
                                         (mailbox-labels-place-words)))))))
         (first-line (+ 2 (count #\Newline (pieces-text before)))))
    (render-table-list
     list '(("Label" :width 40) ("Messages" :width 8 :right t))
     (loop for set in sets
           collect (cons (mailbox-label-set-journal set)
                         (loop for (label . count) in (mailbox-label-set-labels set)
                               collect (list (mailbox-label-set-journal set) label count))))
     (lambda (member mark section)
       (declare (ignore section))
       (list (cons (second member) (list-mark-face mark)) (princ-to-string (third member))))
     :sections t
     :section-title (lambda (journal)
                      (mailbox-label-set-words (find journal sets :key #'mailbox-label-set-journal :test #'string=)))
     :marked-by (lambda (member section) (declare (ignore section)) (cons (first member) (second member)))
     :before before
     :point (if (eq point :first) first-line point)
     :refill (lambda (list) (mailbox-labels-render list :keep)))))

(define-command outlook-label-manager ()
  "List the labels kept for mailboxes (\\[outlook-label]), a section for each
mailbox with the labels on its messages, to rename, delete, forget, copy out
and read back; the heading says where they are kept and how much room they
take. \\<outlook-label-manager-mode>\\[list-visit] on a label opens its
mailbox, \\[outlook-label-rename] renames a label,
\\[list-mark-for-deletion] and \\[list-execute] delete labels,
\\[outlook-labels-forget] forgets a mailbox's labels, \\[outlook-labels-export]
and \\[outlook-labels-import] copy them out and in. The labels are read in the
background."
  (let ((list (buffer-in-mode "*Mailbox labels*" 'outlook-label-manager-mode)))
    (clrhash *mailbox-label-files*)
    (show-buffer list)
    (mailbox-labels-render list)
    (mailbox-labels-start list "reading the labels" nil)))

(defmethod list-entry-noun ((mode outlook-label-manager-mode))
  "A label of a mailbox."
  "label")

;;; Changing a journal

(defun mailbox-change-labels (journal change &key name)
  "Change the labels of the mailbox whose journal is named JOURNAL while
holding its lock: CHANGE, a function of the table of what the journal leaves
\(MAILBOX-REPLAY-LABELS), gives the lines to add, which are written with a
line telling when the labels changed (MAILBOX-META-TEXT), NAME the mailbox's
file name when the journal tells none. Returns how many lines CHANGE gave, or
NIL when the lock was not had in a few seconds. It waits for the lock, so it
is called in the background."
  (let ((path (path-join (mailbox-label-directory) journal)))
    (call-with-state-lock
     path
     (lambda ()
       (let* ((lines (mailbox-journal-lines path))
              (new (funcall change (mailbox-replay-labels lines)))
              (meta (mailbox-journal-meta lines)))
         (when new
           (mailbox-append-lines
            path (append new (list (mailbox-meta-text (list (or (first meta) name "") (or (second meta) "")
                                                            (or (third meta) 0) (mailbox-journal-sum journal)
                                                            (get-universal-time)))))))
         (length new)))
     :wait 5)))

(defun mailbox-rename-label-lines (table old new)
  "The journal lines that rename the label OLD to NEW on every message of
TABLE, a journal's labels: a message that has NEW already only loses OLD."
  (loop for identity being the hash-keys of table using (hash-value labels)
        when (member old labels :test #'string=)
          collect (mailbox-label-line "-" old identity)
          and unless (member new labels :test #'string=)
                collect (mailbox-label-line "+" new identity)))

(defun mailbox-delete-label-lines (table label)
  "The journal lines that take LABEL off every message of TABLE, a journal's
labels."
  (loop for identity being the hash-keys of table using (hash-value labels)
        when (member label labels :test #'string=)
          collect (mailbox-label-line "-" label identity)))

(defun mailbox-import-lines (table lines)
  "The journal lines that put on the messages of TABLE, a journal's labels,
those labels LINES (+, a tab, the label, a tab, the identity) give that they
lack, each label cleaned as one typed; lines that make no sense are passed
over."
  (let ((seen (make-hash-table :test 'equal)) (new '()))
    (dolist (line lines (nreverse new))
      (let* ((first (position #\Tab line))
             (second (and first (position #\Tab line :start (1+ first))))
             (label (and second (string= "+" line :end2 first) (mailbox-clean-label (subseq line (1+ first) second))))
             (identity (and second (subseq line (1+ second)))))
        (when (and label (plusp (length identity)) (position #\Tab identity)
                   (not (member label (gethash identity table) :test #'string=))
                   (not (gethash (cons label identity) seen)))
          (setf (gethash (cons label identity) seen) t)
          (push (mailbox-label-line "+" label identity) new))))))

;;; The commands

(defun mailbox-labels-line ()
  "What the current line of the label manager is, as two values: the journal
of the mailbox it is of or in, and the label it shows, NIL on a mailbox's own
line; NIL, said, on no mailbox's line."
  (let ((entry (list-entry-at)))
    (case (and (consp entry) (first entry))
      (:section (values (second entry) nil))
      (:member (values (third entry) (second (second entry))))
      (t (message "No mailbox on this line") nil))))

(defun mailbox-label-set-named (journal)
  "The MAILBOX-LABEL-SET of the journal named JOURNAL as the manager shows it;
NIL when there is none."
  (find journal *mailbox-label-sets* :key #'mailbox-label-set-journal :test #'string=))

(defun mailbox-labels-gone-words ()
  "What to say when the mailbox a command was given for has no labels in what
the manager shows: that they are still being read, or that it has none."
  (if (mailbox-labels-working (current-buffer))
      "Still reading the labels: try again in a moment"
      "That mailbox has no labels any more"))

(defun mailbox-label-set-title (set)
  "How a question names the mailbox of SET: its file's name, or the start of
its sum when it has none noted."
  (let ((sum (mailbox-label-set-sum set)))
    (or (let ((name (first (mailbox-label-set-meta set)))) (and name (plusp (length name)) name))
        (subseq sum 0 (min 8 (length sum))))))

(defun mailbox-label-open (journal &key other)
  "Open the mailbox whose labels are kept in the journal named JOURNAL, as it
was last opened, when its file is there, in another window with OTHER;
otherwise say why not."
  (let* ((set (mailbox-label-set-named journal))
         (path (and set (second (mailbox-label-set-meta set)))))
    (cond ((null set) (message "~a" (mailbox-labels-gone-words)))
          ((or (null path) (zerop (length path)))
           (message "Where ~a was opened is not known: open it from its directory" (mailbox-label-set-title set)))
          ((eq :gone (gethash path *mailbox-label-files*)) (message "~a is not there now" path))
          ((null (gethash path *mailbox-label-files*)) (message "Still looking for ~a: try again" path))
          (other (visit-path path :show #'display-buffer))
          (t (visit-path path)))))

(define-command outlook-label-open ()
  "Open the mailbox of the section the current line is in, as it was last
opened, when its file is there; otherwise say why not."
  (let ((journal (mailbox-labels-line)))
    (when journal (mailbox-label-open journal))))

(defmethod visit-entry ((mode outlook-label-manager-mode) list entry other)
  "Open the mailbox the label on the line is of, when its file is there."
  (declare (ignore list))
  (mailbox-label-open (first entry) :other other))

(define-command outlook-label-rename ()
  "Rename the label on the current line, or each label marked, asking for the
new name of each: on every message of its mailbox that carries it. A message
that has the new name already only loses the old. The labels of other
mailboxes are not changed. It is done in the background."
  (let ((list (current-buffer)) (renames '()) (keys (list-acted-on (current-buffer))))
    (if (or (null keys) (not (mailbox-labels-free-p list)))
        (unless keys (message "No label on this line"))
        (progn
          (dolist (key keys)
            (let* ((old (cdr key))
                   (new (mailbox-clean-label (read-string (format nil "Rename ~a to: " old)
                                                          :initial old :select-initial t))))
              (if (or (null new) (string= new old))
                  (message "~a is not renamed" old)
                  (push (list (car key) old new) renames))))
          (when renames
            (mailbox-labels-start
             list "renaming"
             (lambda ()
               (let ((done 0) (busy nil))
                 (loop for (journal old new) in (reverse renames)
                       for count = (mailbox-change-labels journal
                                                          (lambda (table) (mailbox-rename-label-lines table old new)))
                       do (if count (incf done count) (setf busy t)))
                 (format nil "Renamed on ~:d message~:p~:[~;; another session was changing some labels~]"
                         done busy)))))))))

(defmethod delete-entries ((mode outlook-label-manager-mode) list keys mark)
  "Take the labels KEYS name, each (JOURNAL . LABEL), off every message of
their mailboxes, after asking; done in the background."
  (declare (ignore mark))
  (let ((sets (mapcar #'mailbox-label-set-named (remove-duplicates (mapcar #'car keys) :test #'string=))))
    (when (and keys (mailbox-labels-free-p list)
               (yes-or-no-p*
                (format nil "~a off every message of ~:[their mailboxes~;~:*~a~]?"
                        (list-question "Take" "label" keys :name-of #'cdr)
                        (and (null (rest sets)) (first sets) (mailbox-label-set-title (first sets))))))
      (mailbox-labels-start
       list "taking labels off"
       (lambda ()
         (let ((count 0))
           (loop for (journal . label) in keys
                 do (incf count (or (mailbox-change-labels journal
                                                           (lambda (table) (mailbox-delete-label-lines table label)))
                                    0)))
           (format nil "Taken off ~:d message~:p" count)))))))

(defun mailbox-forget-labels (journal writers)
  "Delete the journal named JOURNAL while holding its lock, once WRITERS, the
jobs keeping labels of mailboxes open that use it, have ended (a few seconds
at most). The lock file stays, for the sessions that may be waiting for it.
Returns true when the journal is gone."
  (loop repeat 100 while (some #'job-running-p writers) do (sleep 0.05))
  (let ((path (path-join (mailbox-label-directory) journal)))
    (call-with-state-lock path (lambda () (remove-file path) t) :wait 5)))

(define-command outlook-labels-forget ()
  "Forget every label of the mailbox the current line is in, after saying how
many labels and messages that is: its journal is deleted, and the mailbox
starts without labels the next time it is opened. Labels of it put on but not
yet kept are forgotten too. Nothing else is touched, the mailbox least of all.
It is done in the background."
  (let ((journal (mailbox-labels-line)) (list (current-buffer)))
    (when journal
      (let ((set (mailbox-label-set-named journal)))
        (when (and set (mailbox-labels-free-p list)
                   (yes-or-no-p*
                    (format nil "Forget the ~:d label~:p on ~:d message~:p of ~a~:[~; (with deleted items)~]?"
                            (length (mailbox-label-set-labels set)) (mailbox-label-set-messages set)
                            (mailbox-label-set-title set) (mailbox-label-set-deleted set))))
          (let ((writers '()))
            (loop for (buffer name nil writer) in (mailbox-open-lists)
                  when (string= name journal)
                    do (setf (mailbox-job-labels-pending (buffer-local buffer 'mailbox-job)) nil)
                       (when writer (push writer writers)))
            (mailbox-labels-start
             list "forgetting"
             (lambda ()
               (if (mailbox-forget-labels journal writers)
                   (format nil "The labels of ~a are forgotten" (mailbox-label-set-title set))
                   "The labels are being changed by another session"))
             :forgetting journal)))
        (unless set (message "~a" (mailbox-labels-gone-words)))))))

(defparameter *mailbox-labels-file-heading* "# tuatara-outlook-labels 1"
  "The first line of a file of a mailbox's labels copied out.")

(defun mailbox-labels-file-text (set)
  "The text of the file holding the labels of SET, a MAILBOX-LABEL-SET: a
first line saying what it is, lines beginning # with the mailbox's SHA-256
sum, its file's name and whether deleted items were read, then a line for
each label on each message, + a tab, the label, a tab and the message's
identity (its folder, Message-ID and the sum of its headers), as the journal
has them."
  (let ((table (mailbox-replay-labels (mailbox-journal-lines (path-join (mailbox-label-directory)
                                                                        (mailbox-label-set-journal set))))))
    (with-output-to-string (out)
      (format out "~a~%# sha256 ~a~%# file ~a~%# deleted-items ~:[no~;yes~]~%" *mailbox-labels-file-heading*
              (mailbox-label-set-sum set)
              (mailbox-clean-file-name (or (first (mailbox-label-set-meta set)) ""))
              (mailbox-label-set-deleted set))
      (loop for identity being the hash-keys of table using (hash-value labels)
            do (dolist (label labels) (write-string (mailbox-label-line "+" label identity) out))))))

(defun mailbox-clean-file-name (text)
  "TEXT, a mailbox's file name from outside, as the journals keep it: each
control character made a space, and no more than 255 characters."
  (let ((clean (substitute-if #\Space (lambda (c) (or (< (char-code c) 32) (= (char-code c) 127))) text)))
    (subseq clean 0 (min 255 (length clean)))))

(defun mailbox-export-labels (set path)
  "Write the labels of SET into the file PATH names, in its directory under its
name or where that is taken one with (2) and so on, nothing overwritten and
nothing in an evidence directory; words saying what was done. Looks at the
directory, so it is called in the background."
  (multiple-value-bind (directory name) (mailbox-new-file-target path "written")
    (cond ((null directory) name)
          ((zerop (mailbox-label-set-messages (mailbox-read-label-set (mailbox-label-set-journal set) 0)))
           (format nil "~a has no labels to write: nothing written" (mailbox-label-set-title set)))
          (t (let ((text (string-octets (mailbox-labels-file-text set))))
               (format nil "Labels written to ~a"
                       (call-with-free-file directory name
                                            (lambda (fd written)
                                              (write-octets fd text :path written)
                                              written))))))))

(define-command outlook-labels-export ()
  "Write the labels of the mailbox the current line is in into a file, in the
directory and under the name asked for (the mailbox's name and .labels at
first): a plain text file holding the mailbox's SHA-256 sum, its file's name
and, for each message, its identity and its labels, to be read back by
\\<outlook-label-manager-mode>\\[outlook-labels-import]. A name taken gets (2)
and so on, nothing is overwritten, and nothing is written in an evidence
directory. It is written in the background."
  (let ((journal (mailbox-labels-line)) (list (current-buffer)))
    (when journal
      (let ((set (mailbox-label-set-named journal)))
        (if (or (null set) (not (mailbox-labels-free-p list)))
            (unless set (message "~a" (mailbox-labels-gone-words)))
            (let ((path (read-native-path (format nil "Write the labels of ~a into: " (mailbox-label-set-title set))
                                          :kind :any :writing t
                                          :initial (format nil "~a.labels" (mailbox-label-set-title set)))))
              (if (null path)
                  (message "No file named: nothing written")
                  (mailbox-labels-start list "writing a file" (lambda () (mailbox-export-labels set path))))))))))

(defun mailbox-labels-file-header (lines key)
  "What the line `# KEY value' among the first LINES of a file of labels says,
as text; NIL when there is none."
  (let ((prefix (format nil "# ~a " key)))
    (loop for line in (rest lines) while (uiop:string-prefix-p "# " line)
          when (uiop:string-prefix-p prefix line) return (subseq line (length prefix)))))

(defun mailbox-read-labels-file (path)
  "What the file at PATH, made by MAILBOX-LABELS-FILE-TEXT, holds, as four
values: the mailbox's SHA-256 sum (exactly 64 lowercase hexadecimal digits, else
the file is refused), its file's name (cleaned, MAILBOX-CLEAN-FILE-NAME),
whether deleted items were read, and the lines putting labels on messages. NIL,
with why as a second value, when it is not such a file or is larger than 32 MiB.
Its text is only read, as data."
  (let ((text (file-text path :limit (* 32 1024 1024) :too-large (lambda (name size)
                                                                   (declare (ignore name size))
                                                                   nil))))
    (if (null text)
        (values nil "it is too large to be labels")
        (let* ((lines (remove "" (uiop:split-string text :separator (string #\Newline)) :test #'string=))
               (sum (mailbox-labels-file-header lines "sha256")))
          (if (and (equal (first lines) *mailbox-labels-file-heading*) (mailbox-sum-p sum))
              (values sum (mailbox-clean-file-name (or (mailbox-labels-file-header lines "file") ""))
                      (equal "yes" (mailbox-labels-file-header lines "deleted-items"))
                      (remove-if (lambda (line) (uiop:string-prefix-p "#" line)) (rest lines)))
              (values nil "it is not a file of labels copied out"))))))

(defun mailbox-import-labels (path here)
  "Read the file of labels at PATH and add its labels to the mailbox whose sum
it holds; words saying what was done. HERE, the sum of the mailbox the manager's
cursor was on or NIL, must be the file's when given. Reads the whole file, so
it is called in the background."
  (multiple-value-bind (sum name deleted lines) (mailbox-read-labels-file path)
    (cond ((null sum) (format nil "~a: ~a" (native-file-name path) name))
          ((and here (string/= sum here))
           (format nil "~a holds the labels of the mailbox with sum ~a, not of the one on the line: nothing read"
                   (native-file-name path) (subseq sum 0 8)))
          (t (let ((count (mailbox-change-labels (format nil "~a~:[~;~a~]" sum deleted *mailbox-with-deleted-ending*)
                                                 (lambda (table) (mailbox-import-lines table lines))
                                                 :name name)))
               (if count
                   (format nil "~:d label~:p added~@[ to ~a~]" count (and (plusp (length name)) name))
                   "The labels are being changed by another session"))))))

(define-command outlook-labels-import ()
  "Read a file of a mailbox's labels, written by
\\<outlook-label-manager-mode>\\[outlook-labels-export], and add them to the
mailbox whose SHA-256 sum it holds: those it has already stay, and none is
taken away. On a mailbox's line, only that mailbox may be the file's; a file
of another is refused, said. Anywhere else the mailbox is the file's own,
which gets its labels again after they were forgotten. The file is read in the
background."
  (let* ((entry (list-entry-at))
         (here (and (consp entry) (member (first entry) '(:section :member))
                    (mailbox-label-set-named (if (eq (first entry) :section) (second entry) (third entry)))))
         (path (and (mailbox-labels-free-p (current-buffer)) (read-native-path "Read labels from: " :kind :file)))
         (sum (and here (mailbox-label-set-sum here))))
    (when path
      (mailbox-labels-start (current-buffer) "reading a file" (lambda () (mailbox-import-labels path sum))))))
