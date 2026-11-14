;;;; mail.lisp --- A mail item as the outlook package reads it: its parts, settings, encodings and dates.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A message read from an Outlook item (.msg), a saved mail (.eml) or a
;;;; mailbox (.pst, .ost, by readpst) becomes one MAIL, shown one way
;;;; whichever it came from (view.lisp). Its texts come in many encodings,
;;;; named by a charset (MIME) or a code page (Outlook); both are read
;;;; leniently, an octet that is not of the encoding shown as such, never an
;;;; error.

(in-package #:outlook)

(define-package-setting *outlook-readpst-program* "readpst"
  "The program that reads Outlook mailboxes into separate messages, shut in the
sandbox.
The mailboxes are personal storage table (.pst) or offline storage table
\(.ost) files. The program is readpst, of the libpst package. Set it in the
init file after the outlook package is loaded.
- a name: looked for in the system's own directories of programs.
- a path: that program."
  :values string)

(define-package-setting *outlook-page-program* "weasyprint"
  "The program that draws a message written in HTML as it looks, as pages, shut
in the sandbox.
It is WeasyPrint. It runs no scripts, and shut in it fetches nothing from the
internet. Set it in the init file after the outlook package is loaded.
- a name: looked for in the system's own directories of programs.
- a path: that program."
  :values string)

(define-package-setting *outlook-include-deleted* t
  "Whether the list of a mailbox (.pst, .ost) shows the items it still holds that
were deleted.
They are listed in the folders they were deleted from, beside the others.
- t: list them.
- nil: list only the items the mailbox holds as not deleted."
  :values boolean)

(define-package-setting *outlook-largest-file* (* 256 1024 1024)
  "The most octets an Outlook item (.msg) or a saved mail (.eml) may have to be
read as a message.
An attachment or a part of one may hold as many. A larger one is opened as any
file.
- a whole number, 1 or more: the octets, 256 megabytes (MB) by default."
  :values (integer 1 *))

(define-package-setting *outlook-nesting-limit* 8
  "How deep messages attached to messages are read.
An attachment that is a message is shown as one up to this many levels down,
deeper ones listed as not read. A message of parts within parts is read as
deep as twice this.
- a whole number, 0 or more: the levels, 8 by default."
  :values (integer 0 *))

(define-package-setting *outlook-mailbox-seconds* 3600
  "The most seconds readpst may take reading a mailbox (.pst, .ost).
After them it is stopped and what it wrote by then is listed.
- a whole number, 1 or more: the seconds, 3600 (an hour) by default."
  :values (integer 1 *))

(define-package-setting *outlook-mailbox-most-files* 1000000
  "The most files readpst writes that a mailbox's (.pst, .ost) list takes in.
Past them the rest are left out, and the list says so. Raise it to list a
larger mailbox whole.
- a whole number, 1 or more: the files, a million by default."
  :values (integer 1 *))

(define-package-setting *outlook-search-text-limit* (* 16 1024 1024)
  "The most characters of a message of a mailbox (.pst, .ost) its list's search
looks through.
They are counted over its headers, the text of its text parts and its
attachments' names. A larger message is searched as far as that, and the list
says how many were.
- a whole number, 1 or more: the characters, 16 million by default."
  :values (integer 1 *))

(define-package-setting *outlook-mailbox-space* (* 32 1024 1024 1024)
  "The most octets readpst may write reading a mailbox (.pst, .ost).
Its messages go in your cache directory. It is stopped at the limit, as it is
when the drive has less than 1 gigabyte (GB) free, and what it wrote by then
is listed.
- a whole number, 1 or more: the octets, 32 GB by default."
  :values (integer 1 *))

(define-package-setting *outlook-preview* t
  "Whether the list of a mailbox (.pst, .ost) shows the message on the current
line in a window beside it.
The message is read as any message is, nothing in it run and nothing it points
to fetched, and follows the cursor from line to line.
\\<mailbox-mode>\\[list-toggle-preview] turns it on or off in one list.
- t: the preview is on.
- nil: the preview is off until turned on."
  :values boolean)

;;; A message

(defstruct (mail (:copier nil))
  "A mail item read. FROM, TO, carbon copy (CC) and blind carbon copy (BCC) are
as written to be read (names with their addresses). SUBJECT is its subject.
SENT and RECEIVED are universal times or NIL, with DATE-TEXT the date as the
item writes it when it could not be read as one. MESSAGE-ID is its message
identifier. DETAILS is more of what the item says of itself, each (LABEL .
TEXT). BODY is its text to show, read from BODY-KIND (:plain, :html, :rtf, or
NIL for none). HTML is a function giving the HTML its body is written in, when
it has one, read only when asked. HEADERS is its internet headers as they
came, or NIL. ATTACHMENTS is each a MAIL-ATTACHMENT. NOTES is what could not
be read of it, in words."
  from to cc bcc subject sent received date-text message-id
  (details '()) body body-kind html headers (attachments '()) (notes '()))

(setf (documentation 'mail-p 'function) "True when OBJECT is a MAIL.")

(defstruct (mail-attachment (:copier nil))
  "An attachment of a MAIL. NAME is as the item gives it. OCTETS is its
contents, or PART, the part of internet mail they are decoded from when
asked (MAIL-ATTACHMENT-CONTENTS), or MESSAGE, a MAIL, when it is a message
itself. MIME is the Multipurpose Internet Mail Extensions (MIME) type the
item says it is. HIDDEN is true when the item hides it from its reader.
CONTENT-ID is set when the body shows it in place. WHY is what it is in words
when it holds neither octets nor a message read (one too deep, an embedded
object). COPY-PATH is the path of the private copy of its octets once one is
made."
  name octets part message mime hidden content-id why copy-path)

(setf (documentation 'mail-attachment-p 'function) "True when OBJECT is a MAIL-ATTACHMENT.")

(defun mail-note (mail format &rest arguments)
  "Add to MAIL's notes what could not be read, FORMAT with ARGUMENTS."
  (setf (mail-notes mail) (append (mail-notes mail) (list (apply #'format nil format arguments)))))

(defun mail-detail (mail label text)
  "Add LABEL with TEXT to what MAIL says of itself, when TEXT is not NIL or empty."
  (when (and text (plusp (length (princ-to-string text))))
    (setf (mail-details mail) (append (mail-details mail) (list (cons label (princ-to-string text)))))))

(defun mail-person (name address &optional type)
  "A sender or a recipient as written to be read, as NAME <ADDRESS>, the address
with its TYPE when that is not Simple Mail Transfer Protocol (SMTP) (Exchange
\(EX): /O=...); either alone when the other is missing or the same; NIL for
neither."
  (let ((name (and name (plusp (length (string-trim " " name))) (string-trim " " name)))
        (address (and address (plusp (length (string-trim " " address)))
                      (if (and type (plusp (length type)) (string-not-equal type "SMTP"))
                          (format nil "~a: ~a" type (string-trim " " address))
                          (string-trim " " address)))))
    (cond ((and name address (string-not-equal name address)) (format nil "~a <~a>" name address))
          (t (or address name)))))

;;; Encodings

(defun mail-codepage-charset (codepage)
  "The charset Outlook's code page number CODEPAGE stands for, as
CHARSET-TEXT takes it (1252 windows-1252, 65001 utf-8); NIL for none."
  (and (integerp codepage)
       (case codepage
         (65001 "utf-8") ((20127 28591) "iso-8859-1") (1200 "utf-16le") (1201 "utf-16be") (20866 "koi8-r")
         (21866 "koi8-u") (932 "shift_jis") (936 "gbk") (51932 "euc-jp") (50220 nil)
         (t (cond ((<= 28592 codepage 28606) (format nil "iso-8859-~d" (- codepage 28590)))
                  ((<= 1250 codepage 1258) (format nil "windows-~d" codepage))
                  ((< 0 codepage 10000) (format nil "cp~d" codepage)))))))

;;; Times

(defun filetime-universal-time (filetime)
  "The universal time of the file time (FILETIME), a count of 100-nanosecond
steps since 1601 as Windows keeps times, to the second; NIL for none (0) or one
past the year 9999."
  (and (integerp filetime) (< 0 filetime #x24C85A5ED1C04000)
       (- (floor filetime 10000000) 9435484800)))
