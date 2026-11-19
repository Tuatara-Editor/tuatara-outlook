;;;; pst-format.lisp --- What reading and writing Outlook mailboxes (.pst, .ost) share: numbers, checks, codes.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Microsoft's [MS-PST] (revision 11.2, February 2025; section numbers below
;;;; are of it) lays out a mailbox in three layers: the node database (NDB),
;;;; the lists, tables and properties (LTP) on it, and the messaging layer.
;;;; What both the writer (pst.lisp) and the reader (pst-read.lisp) need is
;;;; here: numbers kept least significant octet first, the cyclic redundancy
;;;; check (CRC) of pages and blocks, the two ways a data block may be encoded
;;;; (NDB_CRYPT_PERMUTE and NDB_CRYPT_CYCLIC, 5.1 and 5.2), the signature of a
;;;; page or block, the sizes of property types, a message as properties, and
;;;; the map naming its named properties.

(in-package #:outlook)

;;; Octets

(deftype pst-octets () '(simple-array (unsigned-byte 8) (*)))

(defun pst-octets (size)
  "A new vector of SIZE octets, each 0."
  (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

(defun pst-put (octets at value count)
  "Store the unsigned integer VALUE in COUNT octets of OCTETS from AT, least
significant first, as every number in a mailbox is kept; return OCTETS."
  (dotimes (i count octets)
    (setf (aref octets (+ at i)) (ldb (byte 8 (* 8 i)) value))))

(defun pst-number (value count)
  "The unsigned integer VALUE in COUNT octets, least significant first."
  (pst-put (pst-octets count) 0 value count))

(defun pst-join (parts)
  "The octet vectors PARTS, a list, one after another in one vector."
  (let ((out (pst-octets (reduce #'+ parts :key #'length))) (at 0))
    (dolist (part parts out)
      (replace out part :start1 at)
      (incf at (length part)))))

(defun pst-text (string)
  "STRING in UTF-16, least significant octet first, with no null after it: a
string property of a Unicode mailbox (PtypString, [MS-OXCDATA] 2.11.1). A
character past U+FFFF takes two units (a surrogate pair)."
  (let ((units '()))
    (loop for char across string
          for code = (char-code char)
          do (if (> code #xFFFF)
                 (let ((rest (- code #x10000)))
                   (push (+ #xD800 (ash rest -10)) units)
                   (push (+ #xDC00 (logand rest #x3FF)) units))
                 (push code units)))
    (let ((out (pst-octets (* 2 (length units)))))
      (loop for unit in (nreverse units) for at from 0 by 2 do (pst-put out at unit 2))
      out)))

(defun pst-utf-16-text (octets &key (start 0) (end (length octets)))
  "OCTETS from START below END as UTF-16 text, least significant octet first,
as a Unicode mailbox keeps its strings: a surrogate pair as the one character
it stands for, a lone surrogate as U+FFFD, an odd octet at the end left out."
  (declare (type pst-octets octets) (type fixnum start end) (optimize speed))
  (let ((out (make-string (floor (max 0 (- end start)) 2))) (count 0) (at start))
    (declare (type fixnum count at))
    (loop while (< (1+ at) end)
          do (let ((unit (logior (aref octets at) (ash (aref octets (1+ at)) 8))))
               (incf at 2)
               (setf (schar out count)
                     (cond ((not (<= #xD800 unit #xDFFF)) (code-char unit))
                           ((and (< unit #xDC00) (< (1+ at) end)
                                 (<= #xDC00 (logior (aref octets at) (ash (aref octets (1+ at)) 8)) #xDFFF))
                            (let ((low (logior (aref octets at) (ash (aref octets (1+ at)) 8))))
                              (incf at 2)
                              (code-char (+ #x10000 (ash (- unit #xD800) 10) (- low #xDC00)))))
                           (t (code-char #xFFFD))))
               (incf count)))
    (subseq out 0 count)))

(defun pst-groups (list size)
  "LIST cut into lists of at most SIZE elements, in order; NIL for none."
  (loop while list
        collect (loop repeat size while list collect (pop list))))

(defun pst-filetime (time)
  "The universal time TIME as Windows keeps times (a FILETIME): 100-nanosecond
steps since 1601, the inverse of FILETIME-UNIVERSAL-TIME."
  (* (+ time 9435484800) 10000000))

;;; Checks and encoding

(defparameter *pst-crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8) (setf c (if (logbitp 0 c) (logxor (ash c -1) #xEDB88320) (ash c -1))))
        (setf (aref table n) c))))
  "The table of the cyclic redundancy check (CRC) of every check in a mailbox,
[MS-PST] 5.3's CrcTableOffset32: that of the reflected polynomial #xEDB88320.")

(defparameter *pst-crc-tables*
  (let ((tables (make-array 1024 :element-type '(unsigned-byte 32))))
    (replace tables *pst-crc-table*)
    (loop for k from 1 below 4
          do (dotimes (n 256)
               (let ((previous (aref tables (+ (* 256 (1- k)) n))))
                 (setf (aref tables (+ (* 256 k) n))
                       (logxor (ash previous -8) (aref *pst-crc-table* (logand previous #xFF)))))))
    tables)
  "*PST-CRC-TABLE* and three tables after it, each the one before taken one
octet further, so that a CRC takes in four octets at a step ([MS-PST] 5.3's
CrcTableOffset40 to 64 do as much).")

(defun pst-crc (octets &key (start 0) (end (length octets)))
  "The 32-bit CRC of OCTETS from START below END as [MS-PST] 5.3 computes it:
begun from 0 and not inverted at the end, unlike the CRC-32 of zip files;
four octets at a step."
  (declare (type pst-octets octets) (type fixnum start end) (optimize speed))
  (let ((crc 0) (tables *pst-crc-tables*) (i start))
    (declare (type (unsigned-byte 32) crc) (type (simple-array (unsigned-byte 32) (1024)) tables) (type fixnum i))
    (loop while (<= (+ i 4) end)
          do (setf crc (logxor crc (aref octets i) (ash (aref octets (+ i 1)) 8) (ash (aref octets (+ i 2)) 16)
                               (ash (aref octets (+ i 3)) 24))
                   crc (logxor (aref tables (+ 768 (logand crc #xFF)))
                               (aref tables (+ 512 (logand (ash crc -8) #xFF)))
                               (aref tables (+ 256 (logand (ash crc -16) #xFF)))
                               (aref tables (ash crc -24))))
             (incf i 4))
    (loop while (< i end)
          do (setf crc (logxor (aref tables (logand (logxor crc (aref octets i)) #xFF)) (ash crc -8)))
             (incf i))
    crc))

(defparameter *pst-permutation*
  (coerce '(65 54 19 98 168 33 110 187 244 22 204 4 127 100 232 93
            30 242 203 42 116 197 94 53 210 149 71 158 150 45 154 136
            76 125 132 63 219 172 49 182 72 95 246 196 216 57 139 231
            35 59 56 142 200 193 223 37 177 32 165 70 96 78 156 251
            170 211 86 81 69 124 85 0 7 201 43 157 133 155 9 160
            143 173 179 15 99 171 137 75 215 167 21 90 113 102 66 191
            38 74 107 152 250 234 119 83 178 112 5 44 253 89 58 134
            126 206 6 235 130 120 87 199 141 67 175 180 28 212 91 205
            226 233 39 79 195 8 114 128 207 176 239 245 40 109 190 48
            77 52 146 213 14 60 34 50 229 228 249 159 194 209 10 129
            18 225 238 145 131 118 227 151 230 97 138 23 121 164 183 220
            144 122 92 140 2 166 202 105 222 80 26 17 147 185 82 135
            88 252 237 29 55 73 27 106 224 41 51 153 189 108 217 148
            243 64 84 111 240 198 115 184 214 62 101 24 68 31 221 103
            16 241 12 25 236 174 3 161 20 123 169 11 255 248 163 192
            162 1 247 46 188 36 104 117 13 254 186 47 181 208 218 61)
          '(simple-array (unsigned-byte 8) (256)))
  "The octet each octet of a data block is written as when the mailbox is
permuted: the first table of mpbbCrypt, [MS-PST] 5.1 (mpbbR).")

(defun pst-permute (octets)
  "OCTETS permuted as NDB_CRYPT_PERMUTE writes a data block ([MS-PST] 5.1), a
new vector."
  (declare (type pst-octets octets) (optimize speed))
  (let ((out (pst-octets (length octets))) (table *pst-permutation*))
    (declare (type (simple-array (unsigned-byte 8) (256)) table))
    (dotimes (i (length octets) out)
      (setf (aref out i) (aref table (aref octets i))))))

(defparameter *pst-unpermutation*
  (let ((inverse (make-array 256 :element-type '(unsigned-byte 8))))
    (dotimes (i 256 inverse) (setf (aref inverse (aref *pst-permutation* i)) i)))
  "The octet each octet of a permuted data block was before it was permuted:
the third table of mpbbCrypt, [MS-PST] 5.1 (mpbbI), the inverse of
*PST-PERMUTATION*.")

(defparameter *pst-cyclic-table*
  (coerce '(20 83 15 86 179 200 122 156 235 101 72 23 22 21 159 2
            204 84 124 131 0 13 12 11 162 98 168 118 219 217 237 199
            197 164 220 172 133 116 214 208 167 155 174 154 150 113 102 195
            99 153 184 221 115 146 142 132 125 165 94 209 93 147 177 87
            81 80 128 137 82 148 79 78 10 107 188 141 127 110 71 70
            65 64 68 1 17 203 3 63 247 244 225 169 143 60 58 249
            251 240 25 48 130 9 46 201 157 160 134 73 238 111 77 109
            196 45 129 52 37 135 27 136 170 252 6 161 18 56 253 76
            66 114 100 19 55 36 106 117 119 67 255 230 180 75 54 92
            228 216 53 61 69 185 44 236 183 49 43 41 7 104 163 14
            105 123 24 158 33 57 190 40 26 91 120 245 35 202 42 176
            175 62 254 4 140 231 229 152 50 149 211 246 74 232 166 234
            233 243 213 47 112 32 242 31 5 103 173 85 16 206 205 227
            39 59 218 186 215 194 38 212 145 29 210 28 34 51 248 250
            241 90 239 207 144 182 139 181 189 192 191 8 151 30 108 226
            97 224 198 193 89 171 187 88 222 95 223 96 121 126 178 138)
          '(simple-array (unsigned-byte 8) (256)))
  "The middle table of mpbbCrypt, [MS-PST] 5.1 (mpbbS), which NDB_CRYPT_CYCLIC
passes each octet through between the other two; it is its own inverse.")

(defun pst-unpermute (octets)
  "OCTETS, a data block NDB_CRYPT_PERMUTE wrote ([MS-PST] 5.1), as they were
before, in place; returns OCTETS."
  (declare (type pst-octets octets) (optimize speed))
  (let ((table *pst-unpermutation*))
    (declare (type (simple-array (unsigned-byte 8) (256)) table))
    (dotimes (i (length octets) octets)
      (setf (aref octets i) (aref table (aref octets i))))))

(defun pst-cyclic (octets key)
  "OCTETS encoded or decoded in place by NDB_CRYPT_CYCLIC ([MS-PST] 5.2), which
is its own inverse, KEY the low 32 bits of the block's identifier; returns
OCTETS."
  (declare (type pst-octets octets) (type (unsigned-byte 32) key) (optimize speed))
  (let ((r *pst-permutation*) (s *pst-cyclic-table*) (i *pst-unpermutation*)
        (w (logand #xFFFF (logxor key (ash key -16)))))
    (declare (type (simple-array (unsigned-byte 8) (256)) r s i) (type (unsigned-byte 16) w))
    (dotimes (k (length octets) octets)
      (let* ((low (logand w #xFF)) (high (ash w -8))
             (b (aref octets k)))
        (declare (type (unsigned-byte 8) low high b))
        (setf b (aref r (logand #xFF (+ b low)))
              b (aref s (logand #xFF (+ b high)))
              b (aref i (logand #xFF (- b high)))
              b (logand #xFF (- b low)))
        (setf (aref octets k) b
              w (logand #xFFFF (1+ w)))))))

(defun pst-signature (ib bid)
  "The signature of a block or page with block identifier (BID) at the file
offset IB, [MS-PST] 5.5: the two halves of their low 32 bits, exclusive-ored
together, exclusive-ored."
  (let ((x (logand (logxor ib bid) #xFFFFFFFF)))
    (logand #xFFFF (logxor (ash x -16) x))))

(defun pst-property-type (tag)
  "The type of the property TAG: its low 16 bits."
  (ldb (byte 16 0) tag))

(defun pst-fixed-size (type)
  "The octets a value of the property TYPE takes, when all its values take as
many ([MS-OXCDATA] 2.11.1); NIL for one of any size (a string, binary
data, an object, several values)."
  (case type
    (#x0002 2) ((#x0003 #x0004 #x000A) 4) (#x000B 1) ((#x0005 #x0006 #x0007 #x0014 #x0040) 8) (#x0048 16)))

(defstruct (pst-message (:copier nil))
  "A message read from a mailbox or to be written in one. PROPERTIES is
each of its properties, (TAG . VALUE), TAG the property's number and type
and VALUE its octets as a property context keeps them (strings in UTF-16
with no null after them, PST-TEXT), or, of an object (type #x000D), the
PST-MESSAGE attached. RECIPIENTS and ATTACHMENTS are each a list of
properties alike. NAMES is what each named property among them (numbered
from #x8000) stands for, each (NUMBER GUID . NAME): GUID the 16 octets of
its property set and NAME a whole number or a string ([MS-PST] 2.4.7)."
  (properties '()) (recipients '()) (attachments '()) (names '()))

(setf (documentation 'pst-message-p 'function) "True when OBJECT is a PST-MESSAGE.")

(defun pst-property (tag value)
  "The property TAG with VALUE as PST-MESSAGE keeps it: a string as UTF-16,
a whole number in the octets the type takes, T or NIL as a boolean, octets
as they are, a PST-MESSAGE as it is."
  (let ((type (pst-property-type tag)))
    (cons tag (cond ((stringp value) (pst-text value))
                    ((eq type #x000B) (pst-number (if value 1 0) 1))
                    ((integerp value)
                     (pst-number (ldb (byte (* 8 (pst-fixed-size type)) 0) value) (pst-fixed-size type)))
                    (t value)))))

(defun pst-value (properties tag)
  "The value of the property TAG in PROPERTIES (PST-MESSAGE's), or NIL."
  (cdr (assoc tag properties)))

;;; Named properties

(defun pst-guid (text)
  "The 16 octets of the globally unique identifier (GUID) TEXT, written
xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx, as a mailbox keeps it: its first three
parts least significant octet first, the rest in order."
  (let ((digits (remove #\- text)))
    (flet ((part (start end) (parse-integer digits :start start :end end :radix 16)))
      (pst-join (list (pst-number (part 0 8) 4) (pst-number (part 8 12) 2) (pst-number (part 12 16) 2)
                      (coerce (loop for at from 16 below 32 by 2 collect (part at (+ at 2))) 'pst-octets))))))

(defparameter *pst-mapi-guid* (pst-guid "00020328-0000-0000-C000-000000000046")
  "The property set PS_MAPI, which a map of named properties numbers 1
\([MS-PST] 2.4.7.1).")

(defparameter *pst-public-strings-guid* (pst-guid "00020329-0000-0000-C000-000000000046")
  "The property set PS_PUBLIC_STRINGS, which a map of named properties
numbers 2.")

(defparameter *pst-common-guid* (pst-guid "00062008-0000-0000-C000-000000000046")
  "The property set PSETID_Common, of properties every kind of item may have.")

(defun pst-name-entry (number guid index &key string)
  "The record (NAMEID, [MS-PST] 2.4.7.1) of a named property named by the
NUMBER in the property set GUID (1 PS_MAPI, 2 PS_PUBLIC_STRINGS, 3 and on
the GUID stream's), its property number #x8000 and INDEX; with STRING, of
one named by a string, NUMBER where the string stream holds it (or, in a
hash bucket, the string's CRC)."
  (pst-join (list (pst-number number 4) (pst-number (logior (ash guid 1) (if string 1 0)) 2) (pst-number index 2))))

(defun pst-name-bucket (entry count)
  "The hash bucket of the named property's record ENTRY among COUNT buckets
\([MS-PST] 2.4.7.5): its first four octets exclusive-ored with the next two,
modulo COUNT."
  (mod (logxor (octets-integer entry 0 4) (octets-integer entry 4 2)) count))

(defun pst-name-table (guids entries strings)
  "The named properties a map of them names ([MS-PST] 2.4.7), given the
octets of its streams of property sets GUIDS, of ENTRIES and of names a
string STRINGS (any NIL for an empty one): a table from each property number
from #x8000 to (GUID . NAME), GUID the 16 octets of its property set and
NAME a whole number or a string. An entry naming a set or a string the
streams do not hold is left out."
  (let ((table (make-hash-table)) (guids (or guids (pst-octets 0))) (entries (or entries (pst-octets 0)))
        (strings (or strings (pst-octets 0))))
    (loop for at from 0 to (- (length entries) 8) by 8
          do (let* ((number (octets-integer entries at 4))
                    (kind (octets-integer entries (+ at 4) 2))
                    (index (octets-integer entries (+ at 6) 2))
                    (set (ash kind -1))
                    (guid (case set
                            (1 *pst-mapi-guid*)
                            (2 *pst-public-strings-guid*)
                            (t (let ((start (* 16 (- set 3))))
                                 (and (>= set 3) (<= (+ start 16) (length guids)) (subseq guids start (+ start 16)))))))
                    (name (if (logbitp 0 kind)
                              (let ((size (and (<= (+ number 4) (length strings)) (octets-integer strings number 4))))
                                (and size (<= (+ number 4 size) (length strings))
                                     (pst-utf-16-text strings :start (+ number 4) :end (+ number 4 size))))
                              number)))
               (when (and guid name)
                 (setf (gethash (+ #x8000 index) table) (cons guid name)))))
    table))

(defun pst-name-streams (names)
  "The streams of a map of named properties ([MS-PST] 2.4.7) naming NAMES, a
table from (GUID . NAME) to the number from #x8000 given it: the octets of
its property sets, of its entries and of its names a string, and its hash
buckets, each (BUCKET . OCTETS) among 251 (2.4.7.5), four values."
  (let ((guids '()) (entries '()) (strings '()) (buckets (make-hash-table)) (at 0))
    (loop for ((guid . name) . number) in (sort (loop for key being the hash-keys of names using (hash-value number)
                                                      collect (cons key number))
                                                #'< :key #'cdr)
          do (let ((set (cond ((equalp guid *pst-mapi-guid*) 1)
                              ((equalp guid *pst-public-strings-guid*) 2)
                              (t (+ 3 (or (position guid guids :test #'equalp)
                                          (progn (setf guids (append guids (list guid))) (1- (length guids))))))))
                   (index (- number #x8000)))
               (if (stringp name)
                   (let* ((text (pst-text name))
                          (piece (pst-join (list (pst-number (length text) 4) text
                                                 (pst-octets (mod (- (length text)) 4)))))
                          (hashed (pst-name-entry (pst-crc text) set index :string t)))
                     (push (pst-name-entry at set index :string t) entries)
                     (push hashed (gethash (pst-name-bucket hashed 251) buckets))
                     (push piece strings)
                     (incf at (length piece)))
                   (let ((entry (pst-name-entry name set index)))
                     (push entry entries)
                     (push entry (gethash (pst-name-bucket entry 251) buckets))))))
    (values (pst-join guids) (pst-join (nreverse entries)) (pst-join (nreverse strings))
            (sort (loop for bucket being the hash-keys of buckets using (hash-value held)
                        collect (cons bucket (pst-join (reverse held))))
                  #'< :key #'car))))

(defun pst-renamed (message names)
  "MESSAGE, a PST-MESSAGE, with each named property among its properties,
its recipients' and its attachments', and those of the messages attached to
it, numbered as NAMES numbers what it stands for: NAMES a table from (GUID .
NAME) to a number from #x8000, given the next number for one it lacks. A
named property whose meaning MESSAGE does not give is left out."
  (labels ((number (own id)
             (let ((name (cdr (assoc id own))))
               (and name (or (gethash name names)
                             (setf (gethash name names) (+ #x8000 (hash-table-count names)))))))
           (renamed (properties own)
             (loop for (tag . value) in properties
                   for id = (ldb (byte 16 16) tag)
                   for new = (if (< id #x8000) id (number own id))
                   when new
                     collect (cons (logior (ash new 16) (pst-property-type tag))
                                   (if (pst-message-p value) (pst-renamed value names) value)))))
    (let ((own (pst-message-names message)))
      (make-pst-message :properties (renamed (pst-message-properties message) own)
                        :recipients (mapcar (lambda (recipient) (renamed recipient own))
                                            (pst-message-recipients message))
                        :attachments (mapcar (lambda (attachment) (renamed attachment own))
                                             (pst-message-attachments message))))))
