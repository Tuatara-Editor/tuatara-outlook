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
MAILBOX-VISIT) and previewed in rimu, their C-c keys bound, and check-health told
what reads them."
  (buttons-in-mode 'outlook-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)")
  (define-keys (mode-keymap 'outlook-view-mode) '(("C-c o" outlook-open-attachment-outside)
                                                  ("C-c t" outlook-view-file-text)
                                                  ("C-c p" outlook-view-pages)))
  (commands-for-mode 'outlook-view-mode "a message (an Outlook item, a saved mail, a message of a mailbox)"
                     'outlook-open-attachment-outside 'outlook-view-file-text 'outlook-view-pages)
  (entry-list-keys 'outlook-mailbox-mode "the list of a mailbox")
  (define-keys (mode-keymap 'outlook-mailbox-mode)
               '(("d" :undefined)
                 ("C-c h" outlook-back-to-directory) ("C-c c" outlook-export)
                 ("C-c +" outlook-label) ("C-c -" outlook-unlabel)
                 ("C-c L" outlook-filter-labels) ("C-c M" outlook-label-manager)))
  (commands-for-mode 'outlook-mailbox-mode "the list of a mailbox" 'outlook-export 'outlook-export-eml
                     'outlook-export-msg 'outlook-export-mbox 'outlook-export-pst
                     'outlook-back-to-directory 'outlook-label 'outlook-unlabel 'outlook-filter-labels
                     'outlook-label-manager)
  (entry-list-keys 'outlook-label-manager-mode "the label manager (\\[outlook-label-manager])" :kind :manager)
  (define-keys (mode-keymap 'outlook-label-manager-mode)
               '(("C-c O" outlook-label-open) ("C-c R" outlook-label-rename) ("C-c f" outlook-labels-forget)
                 ("C-c e" outlook-labels-export) ("C-c i" outlook-labels-import)))
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
