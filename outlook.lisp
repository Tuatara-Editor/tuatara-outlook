;;;; outlook.lisp --- The outlook package: Outlook items, saved mail and mailboxes read safely.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; Needs: Tuatara 0.90.28
;;;;
;;;; One line in the init file loads it: (load-package "outlook").

(defpackage #:outlook
  (:use #:cl #:tuatara)
  (:documentation "Outlook items, saved mail and mailboxes, read safely."))

(in-package #:outlook)

(declare-package-needs
 '("readpst" "reads the mail in .pst files" :required t
   :install "install pst-utils (Debian, Ubuntu) or libpst (Fedora)")
 '("weasyprint" "draws a message written in HTML as it looks" :install "install weasyprint"))

(dolist (file '("mail" "cfb" "mime" "msg" "view" "mailbox" "install"))
  (load-package-file (format nil "~a.lisp" file)))
