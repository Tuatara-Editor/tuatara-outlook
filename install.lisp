;;;; install.lisp --- The outlook package put in place: its views, keys, preview and health.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A package, not part of the editor: loaded by (load-package "outlook") in
;;;; the init file. Once loaded, an Outlook item (.msg), a saved mail (.eml)
;;;; and an Outlook mailbox (.pst, .ost) opened are shown as messages and as
;;;; a list of them; rimu's preview shows them too, and M-x check-health
;;;; says what reads them here.

(in-package #:outlook)

(defun install-outlook ()
  "Have messages and mailboxes visited shown as such (MAIL-VISIT,
MAILBOX-VISIT) and previewed in rimu, their keys bound, and check-health told
what reads them."
  (buttons-in-mode 'outlook-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)")
  (define-key (mode-keymap 'outlook-view-mode) "O" 'outlook-open-attachment-outside)
  (define-key (mode-keymap 'outlook-view-mode) "T" 'outlook-view-file-text)
  (define-key (mode-keymap 'outlook-view-mode) "P" 'outlook-view-pages)
  ;; under C-c o too, as every package's commands are under a prefix of its own
  (define-keys (mode-keymap 'outlook-view-mode) '(("C-c o o" outlook-open-attachment-outside)
                                                  ("C-c o t" outlook-view-file-text)
                                                  ("C-c o p" outlook-view-pages)))
  (label-keys (mode-keymap 'outlook-view-mode) '(("C-c o" "outlook")))
  (commands-for-mode 'outlook-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)"
                     'outlook-open-attachment-outside 'outlook-view-file-text 'outlook-view-pages)
  (entry-list-keys 'outlook-mailbox-mode "the list of a mailbox")
  (define-keys (mode-keymap 'outlook-mailbox-mode)
               '(("h" outlook-back-to-directory) ("C" outlook-export) ("d" :undefined)
                 ("+" outlook-label) ("-" outlook-unlabel)
                 ("L" outlook-filter-labels) ("M" outlook-label-manager)))
  (define-keys (mode-keymap 'outlook-mailbox-mode)
               '(("C-c o h" outlook-back-to-directory) ("C-c o c" outlook-export)
                 ("C-c o +" outlook-label) ("C-c o -" outlook-unlabel)
                 ("C-c o l" outlook-filter-labels) ("C-c o m" outlook-label-manager)))
  (label-keys (mode-keymap 'outlook-mailbox-mode) '(("C-c o" "outlook")))
  (commands-for-mode 'outlook-mailbox-mode "the list of a mailbox" 'outlook-export 'outlook-export-eml
                     'outlook-export-msg 'outlook-export-mbox 'outlook-export-pst
                     'outlook-back-to-directory 'outlook-label 'outlook-unlabel 'outlook-filter-labels
                     'outlook-label-manager)
  (define-keys (mode-keymap 'outlook-label-manager-mode)
               '(("RET" outlook-label-open) ("l" outlook-label-open) ("C-f" outlook-label-open) ("h" quit-window)
                 ("C-b" quit-window) ("n" list-next-entry) ("p" list-previous-entry) ("R" outlook-label-rename)
                 ("D" outlook-labels-forget) ("E" outlook-labels-export) ("I" outlook-labels-import)))
  (define-keys (mode-keymap 'outlook-label-manager-mode)
               '(("C-c o o" outlook-label-open) ("C-c o r" outlook-label-rename) ("C-c o f" outlook-labels-forget)
                 ("C-c o e" outlook-labels-export) ("C-c o i" outlook-labels-import)))
  (label-keys (mode-keymap 'outlook-label-manager-mode) '(("C-c o" "outlook")))
  (commands-for-mode 'outlook-label-manager-mode "the label manager (\\[outlook-label-manager])"
                     'outlook-label-open 'outlook-label-rename 'outlook-labels-forget 'outlook-labels-export
                     'outlook-labels-import 'list-next-entry 'list-previous-entry)
  (entry-preview-keys 'outlook-mailbox-mode "the list of a mailbox")
  (pushnew 'mail-visit *file-views*)
  (pushnew 'mailbox-visit *file-views*)
  (pushnew 'mailbox-visit *sandboxed-file-views*)
  (pushnew 'outlook-preview-text *preview-text-functions*)
  (pushnew 'outlook-health-words *health-decoder-functions*))

(install-outlook)
