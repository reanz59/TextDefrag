
;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/util.lisp,v 1.33 2012-05-04 21:17:44 edi Exp $


(in-package :cl-unicode)

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/util.lisp,v 1.13 2012-05-04 21:17:45 edi Exp $

;(in-package :cl-unicode)
#|
(defvar *this-file* (load-time-value
                     (or #.*compile-file-pathname* *load-pathname*))
  "The location of this source file.  Needed to find the data files.")

(defvar *char-database* nil
  "This will be a vector of CHAR-INFO objects which keeps track of the
information parsed from the Unicode files while the build process
runs.  This \"database\" is not used by CL-UNICODE itself.  It only
serves as a temporary storage during the build process.")

(defun make-empty-char-database ()
  "Creates and returns an array of length +CODE-POINT-LIMIT+
initialized with NILs."
  (make-array +code-point-limit+ :initial-element nil))

(defun initialize-property-symbols ()
  "Clears the hash table *CANONICAL-NAMES* and initializes it with
certain values which might otherwise not be seen when the Unicode
files are parsed."
  (clrhash *canonical-names*)
  (dolist (name '("Cn" "AL" "R" "L" "Decimal" "Digit" "Numeric" "BidiMirrored" "NoncharacterCodePoint"))
    (register-property-symbol name)))

(defun extract-variables (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the
variables from the list BINDINGS where atoms as well as the first
element of BINDINGS are left as is and lists are replaced with their
first elements.  The symbol _ \(underline) is skipped, i.e. not
returned."
  (cons (first bindings)
        (loop for binding in (rest bindings)
              unless (eq binding '_)
              when (listp binding)
              collect (first binding)
              else collect binding)))

(defun extract-types (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the type
spec from the list BINDINGS where it is assumed the for lists the type
spec is the second element and for atoms the type spec is defaulted to
STRING.  The first argument of BINDINGS is skipped, and the type spec
NIL is included whenever the symbol _ \(underline) is seen."
  (loop for binding in (rest bindings)
        when (eq binding '_)
        collect nil
        else when (listp binding)
        collect (second binding)
        else collect 'string))

(defun extract-defaults (bindings)
  "Helper function for the WITH-UNICODE-FILE macro.  Collects the
default values from the list BINDINGS where it is assumed the for
lists the default value is the third element and for atoms the default
value is :ERROR \(meaning to signal an error).  The first argument of
BINDINGS is skipped, and the default value NIL is included whenever
the symbol _ \(underline) is seen."
  (loop for binding in (rest bindings)
        when (eq binding '_)
        collect nil
        else when (and (listp binding) (cddr binding))
        collect (third binding)
        else collect :error))

(defun code-point-range-start (range)
  "Returns the first code point of the code point \"range\" RANGE
where RANGE is either an integer denoting one code point or a cons of
the form (A . B) denoting the code points from A to B \(inclusive)."
  (cond ((atom range) range)
        (t (car range))))

(defun parse-code-point (string)
  "Parses a string which is supposed to be the hexadecimal
representation of one code point or a range of code points \(written
with two dots between them like \"0AE0..0AF3\").  Returns a single
integer \(for one code point) or a cons of two integers \(for a
range)."
  (destructuring-bind (first &optional second)
      (mapcar 'parse-hex (ppcre:split "\\.\\." string))
    (if second (cons first second) first)))

(defgeneric parse-value (value type default)
  (:documentation "Parses the string VALUE coming from a Unicode data
file and converts it according to the type spec TYPE \(a symbol
denoting a type which is not necessarily a Lisp type).  If VALUE is
the empty string, DEFAULT is returned instead unless DEFAULT is :ERROR
in which case an error is signalled.")
  (:method (value type default)
   "The default method for unrecognized type specs."
   (error "Don't know how to parse type ~S." type))
  (:method :around (value type default)
   "The method to take care of default values."
   (cond ((and (string= value "") (eq default :error))
          (error "No value and no default provided"))
         ((string= value "") default)
         (t (call-next-method)))))

(defmethod parse-value (value (type (eql 'string)) default)
  "The method for strings simply returns VALUE."
  value)

(defmethod parse-value (value (type (eql 'boolean)) default)
  "The method for BOOLEAN only accepts the strings \"Y\" and \"N\"."
  (cond ((string= value "Y") t)
        ((string= value "N") nil)
        (t (error "Expected boolean, but got ~S." value))))

(defmethod parse-value (value (type (eql 'symbol)) default)
  "The method for symbol which converts the string to a \"property
symbol\" \(see PROPERTY-SYMBOL) and registers it \(see
REGISTER-PROPERTY-SYMBOL)."
  (register-property-symbol value))

(defmethod parse-value (value (type (eql 'integer)) default)
  "The method for \(decimal) integers."
  (parse-integer value))

(defmethod parse-value (value (type (eql 'hex)) default)
  "The method for hexadecimal integers."
  (parse-hex value))

(defmethod parse-value (value (type (eql 'rational)) default)
  "The method for rationals which are written like Lisp rationals."
  (destructuring-bind (numerator &optional (denominator 1))
      (mapcar 'parse-integer (ppcre:split "/" value))
    (/ numerator denominator)))

(defmethod parse-value (value (type (eql 'age)) default)
  "The method for Unicode \"age\" values which are converted to a
list of two integers, the major and minor version."
  (destructuring-bind (major minor)
      (mapcar 'parse-integer (ppcre:split "\\." value))
    (list major minor)))

(defun parse-one-line (parts &optional types defaults)
  "Parses one line of a Unicode data file and returns a list of Lisp
objects as determined by TYPES and DEFAULTS.  It is assumed that the
line was already split into a list PARTS of individual strings, one
for each value/object.  The elements of TYPES and DEFAULTS are
interpreted as by PARSE-VALUE except that we skip one element of PARTS
for each NIL in TYPES.  The first element of PARTS is always
interpreted as a code point \(range), i.e. TYPES and DEFAULTS only
apply to the rest of PARTS.

Note that a call like \(PARSE-ONE-LINE PARTS) means that just the code
point part is parsed and returned."
  (cons (parse-code-point (first parts))
        (loop for part in (rest parts)
              for type in types
              for default in defaults
              when type 
              collect (parse-value part type default))))

(defconstant +code-point-limit+ #x110000
  "The smallest integer which is not a code point in the Unicode codespace.")

(defvar *canonical-names* (make-hash-table :test 'eq :size 500)
  "A hash tables which maps property symbols \(see PROPERTY-SYMBOL) to
their \"canonical names\", i.e. to strings.")

(defvar *names-to-code-points* (make-hash-table :test 'equalp :size 20000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
character names to their code points.")

(defvar *unicode1-names-to-code-points* (make-hash-table :test 'equalp :size 2000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
Unicode 1.0 character names to their code points.")

(defvar *code-points-to-names* (make-hash-table :size 20000)
  "A hash table which maps code points to the corresponding character
names.")

(defvar *code-points-to-unicode1-names* (make-hash-table :size 2000)
  "A hash table which maps code points to the corresponding Unicode
1.0 character names.")

(defvar *case-mappings* (make-hash-table :size 2100)
  "A hash table which maps code points to three-element lists
containing the lowercase, uppercase, and titlecasse mapping of the
corresponding character \(unless all of them are NIL).")

(defvar *general-categories* nil
  "A list of all property symbols which denote general categories.")

(defvar *scripts* nil
  "A list of all property symbols which denote scripts.")

(defvar *code-blocks* nil
  "A list of all property symbols which denote blocks.")

(defvar *binary-properties* nil
  "A list of all property symbols which denote binary properties.")

(defvar *bidi-classes* nil
  "A list of all property symbols which denote Bidi classes.")

(defvar *property-map* (make-hash-table :test 'equalp :size 1000)
  "A hash table which \(case-insensitively) maps \"canonicalized\"
property names \(including aliases) to the corresponding property
symbols.")

(defvar *property-tests* (make-hash-table :test 'eq :size 360)
  "A hash table which maps property symbols to a test function which
tests for the corresponding property.")

(defvar *property-aliases* (make-hash-table :test 'equalp :size 360)
  "A hash table which maps property names to the long name for
the property.")

(defvar *jamo-short-names* (make-hash-table :size 70)
  "A hash table which maps code points to their Jamo short names.
Needed to compute Hangul syllable names - see COMPUTE-HANGUL-NAME.")

(defvar *hangul-syllables-to-code-points* (make-hash-table :test 'equalp :size 12000)
  "A hash table which \(case-insensitively) maps Hangul syllable name
parts to their code points.")

(defvar *try-unicode1-names-p* t
  "This is the default value for the :TRY-UNICODE1-NAMES-P keyword
argument to CHARACTER-NAMED.")

(defvar *try-abbreviations-p* nil
  "This is the default value for the :TRY-ABBREVIATIONS-P keyword
argument to CHARACTER-NAMED.")

(defvar *scripts-to-try* nil
  "This is the default value for the :SCRIPTS-TO-TRY keyword argument
to CHARACTER-NAMED.")

(defvar *try-hex-notation-p* nil
  "This is the default value for the :TRY-HEX-NOTATION-P keyword
argument to CHARACTER-NAMED.")

(defvar *try-lisp-names-p* nil
  "This is the default value for the :TRY-LISP-NAMES-P keyword
argument to CHARACTER-NAMED.")

(defvar *previous-readtables* nil
  "A stack which holds the previous readtables that have been pushed
here by ENABLE-ALTERNATIVE-CHARACTER-SYNTAX.")

(pushnew :cl-unicode *features*)

;; stuff for Nikodemus Siivola's HYPERDOC
;; see <http://common-lisp.net/project/hyperdoc/>
;; and <http://www.cliki.net/hyperdoc>
;; also used by LW-ADD-ONS

(defvar *hyperdoc-base-uri* "http://weitz.de/cl-unicode/")

(let ((exported-symbols-alist
       (loop for symbol being the external-symbols of :cl-unicode
             collect (cons symbol
                           (concatenate 'string
                                        "#"
                                        (string-downcase symbol))))))
  (defun hyperdoc-lookup (symbol type)
    (declare (ignore type))
    (cdr (assoc symbol
                exported-symbols-alist
                :test #'eq))))
|#

(defun parse-hex (string)
  "Parses STRING as a hexadecimal number."
  (parse-integer string :radix 16))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun canonicalize-name (name)
  "Converts the string NAME into a \"canonicalized\" name which can be
used for unambiguous look-ups by removing all whitespace, hyphens, and
underline characters.

Tries not to remove hyphens preceded by spaces if this could lead to
ambiguities as described in
<http://unicode.org/unicode/reports/tr18/#Name_Properties>.

All CL-UNICODE functions which accept string \"names\" for characters
or properties will canonicalize the name first using this function and
will then look up the name case-insensitively."
  (values (ppcre:regex-replace-all "[ _](-A|O-E)$|[-_\\s]" name
                                   (lambda (match register)
                                     (declare (ignore match))
                                     (cond (register (format nil " ~A" register))
                                           (t "")))
                                   :simple-calls t)))

(defun property-symbol (name)
  "Returns a symbol in the CL-UNICODE-NAMES package \(which is only
used for this purpose) which can stand in for the string NAME in
look-ups.  The symbol's name is the result of \"canonicalizing\" and
then upcasing NAME.

A symbol returned by this function is only really useful and only
actually a property symbol if the second return value is true.

All exported functions of CL-UNICODE which return strings which are
property names return the corresponding property symbol as their
second return value.  All exported functions of CL-UNICODE which
accept property names as arguments will also accept property symbols.

See also PROPERTY-NAME."
  (let ((symbol (intern (string-upcase (canonicalize-name name)) :cl-unicode-names)))
    (values symbol (property-name symbol))))

(defun register-property-symbol (name)
  "Converts NAME to a property symbol using PROPERTY-SYMBOL and
\"registers\" it in the *CANONICAL-NAMES* hash table."
  (let ((symbol (property-symbol name)))
    (setf (gethash symbol *canonical-names*) name)
    symbol))

(defun lookup-property-alias (name)
  "Returns the long-name of the given property alias"
  (gethash (string-upcase (canonicalize-name name)) *property-aliases*))

(defun property-name (symbol)
  "Returns a name \(not \"the\" name) for a property symbol SYMBOL if
it is known to CL-UNICODE.  Note that

  \(STRING= \(PROPERTY-NAME \(PROPERTY-SYMBOL <string>)) <string>)

is not necessarily true even if the property name is not NIL while

  \(EQ \(PROPERTY-SYMBOL \(PROPERTY-NAME <symbol>)) <symbol>)

always holds if there is a property name for <symbol>.

See also PROPERTY-SYMBOL."
  (values (gethash symbol *canonical-names*)))
) ;; END EVAL-WHEN

(defun tree-lookup (code-point tree)
  "Looks up an attribute for CODE-POINT in the binary search tree
TREE.  TREE is a tree as created by BUILD-TREE."
  (labels ((try (node)
             (and node
                  (destructuring-bind (((from . to) . value) left-branch right-branch)
                      node
                    (cond ((< code-point from) (try left-branch))
                          ((> code-point to) (try right-branch))
                          (t value))))))
    (try tree)))

(defgeneric mapping (c position want-code-point-p)
  (:documentation "Returns the simple case mapping for the character C
\(a code point or a Lisp character) in position POSITION where 0 means
lowercase, 1 uppercase, and 2 titlecase.  Returns a character if
WANT-CODE-POINT-P is NIL and a code point otherwise.")
  (:method ((char character) position want-code-point-p)
   (mapping (char-code char) position want-code-point-p))
  (:method ((code-point integer) position want-code-point-p)
   (let* ((mappings (gethash code-point *case-mappings*))
          (code-point (or (nth position mappings) code-point)))
     (if want-code-point-p
       code-point
       (and code-point (code-char code-point))))))

(defun cjk-unified-ideograph-p (code-point)
  "Returns a true value if CODE-POINT is the code point of a CJK
unified ideograph for which we can algorithmically derive the name."
  (or (<= #x3400 code-point #x4db5)
      (<= #x4e00 code-point #x9fc3)
      (<= #x20000 code-point #x2a6d6)))

(defun maybe-compute-cjk-name (code-point)
  "Computes the name for CODE-POINT if CODE-POINT denotes a CJK
unified ideograph the name of which can be algorithmically derived."
  (when (cjk-unified-ideograph-p code-point)
    (format nil "CJK UNIFIED IDEOGRAPH-~X" code-point)))

(defun maybe-find-cjk-code-point (name)
  "Computes the code point for NAME if NAME is the name of a CJK
unified ideograph the name of which can be algorithmically derived."
  (ppcre:register-groups-bind ((#'parse-hex code-point))
      ;; canonicalized
      ("(?i)^CJKUNIFIEDIDEOGRAPH([0-9A-F]{4,5}|10[0-9A-F]{4})$" name)
    (when (cjk-unified-ideograph-p code-point)
      code-point)))

(defmacro define-hangul-constant (name value)
  "Simple helper macro to define some constants needed for the Hangul
algorithm below."
  (flet ((create-symbol (name)
           (intern (format nil "+~:@(~C-~A~)+" (char name 0) (subseq name 1)) :cl-unicode)))
    ;; use EVAL-WHEN so the following definitions can refer to the
    ;; value already
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defconstant ,(create-symbol name) ,value
         ,(format nil "The constant `~A' from chapter 3 of the Unicode book." name)))))

(define-hangul-constant "SBase" #xac00)
(define-hangul-constant "LBase" #x1100)
(define-hangul-constant "VBase" #x1161)
(define-hangul-constant "TBase" #x11a7)
(define-hangul-constant "VCount" 21)
(define-hangul-constant "TCount" 28)
(define-hangul-constant "NCount" (* +V-COUNT+ +T-COUNT+))

(declaim (inline compute-hangul-name))
(defun compute-hangul-name (code-point)
  "Algorithmically derives the Hangul syllable name \(the part behind
\"HANGUL SYLLABLE \") of the character with code point CODE-POINT as
described in section 3.12 of the Unicode book."
  ;;(declare #.*standard-optimize-settings*)
  (declare (fixnum code-point))
  (let* ((s-index (- code-point +S-BASE+))
         (l-value (+ +L-BASE+ (floor s-index +N-COUNT+)))
         (v-value (+ +V-BASE+ (floor (mod s-index +N-COUNT+) +T-COUNT+)))
         (t-value (+ +T-BASE+ (mod s-index +T-COUNT+))))
    (declare (fixnum s-index t-value))
    (format nil "~A~A~@[~A~]"
            (gethash l-value *jamo-short-names*)
            (gethash v-value *jamo-short-names*)
            (and (/= t-value +T-BASE+)
                  (gethash t-value *jamo-short-names*)))))

(defconstant +first-hangul-syllable+ #xac00
  "The code point of the first Hangul syllable the name of which can
be algorithmically derived.")
(defconstant +last-hangul-syllable+ #xd7a3
  "The code point of the last Hangul syllable the name of which can be
algorithmically derived.")

(defun add-hangul-names ()
  "Computes the names for all Hangul syllables and registers them in
the *HANGUL-SYLLABLES-TO-CODE-POINTS* hash table.  Used for
CHARACTER-NAMED."
  ;;(declare #.*standard-optimize-settings*)
  (when *compile-verbose*
    (format t "~&;;; Computing Hangul syllable names")
    (force-output))
  (loop for code-point from +first-hangul-syllable+ to +last-hangul-syllable+
        for name = (compute-hangul-name code-point)
        do (setf (gethash name *hangul-syllables-to-code-points*) code-point)))

(defun hangul-syllable-p (code-point)
  "Returns a true value if CODE-POINT is the code point of a Hangul
syllable for which we can algorithmically derive the name."
  (<= +first-hangul-syllable+ code-point +last-hangul-syllable+))

(defun maybe-compute-hangul-syllable-name (code-point)
  "Computes the name for CODE-POINT if CODE-POINT denotes a Hangul
syllable the name of which can be algorithmically derived."
  (when (hangul-syllable-p code-point)
    (format nil "HANGUL SYLLABLE ~X" (compute-hangul-name code-point))))

(defun maybe-find-hangul-syllable-code-point (name)
  "Computes the code point for NAME if NAME is the name of a Hangul
syllable the name of which can be algorithmically derived."
  (ppcre:register-groups-bind (name)
      ;; canonicalized
      ("(?i)^HANGULSYLLABLE([A-Z]*)$" name)
    (gethash name *hangul-syllables-to-code-points*)))

(defmacro ensure-code-point (c)
  "Helper macro so that C can be treated like a code point even if it
is a Lisp character."
  (with-rebinding (c)
    `(etypecase ,c
       (integer ,c)
       (character (char-code ,c)))))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/char-info.lisp,v 1.6 2012-05-04 21:17:45 edi Exp $

(defclass char-info ()
  ((code-point :initarg :code-point
               :reader code-point
               :type fixnum
               :documentation "The code point of the character.  This
is redundant information, but convenient.")
   (name :initarg :name
         :initform nil
         :reader name
         :type (or string null)
         :documentation "The name of the character - a string.")
   (script :initform nil
           :accessor script*
           :type (or symbol null)
           :documentation "The script the character belongs to - a
property symbol.")
   (code-block :initform nil
               :accessor code-block*
               :type (or symbol null)
               :documentation "The block the character belongs to - a
property symbol.")
   (word-break :initarg :word-break
               ;"other" is value unless indicated otherwise
               :initform (register-property-symbol "Other")
               :accessor word-break*
               :type symbol
               :documentation "The word_break value of the character
- a property symbol")
   (unicode1-name :initarg :unicode1-name
                  :initform nil
                  :reader unicode1-name
                  :type (or string null)
                  :documentation "The Unicode 1.0 name of the
character - a string.")
   (age :initform nil
        :accessor age*
        :type list
        :documentation "The Unicode version this character first
appeared in, a cons of two integers which denote the major and minor
version.")
   (general-category :initarg :general-category
                     ;; this is the default for unassigned characters
                     ;; - see READ-BINARY-PROPERTIES
                     :initform (property-symbol "Cn")
                     :reader general-category*
                     :type symbol
                     :documentation "The general category of this
character - a property symbol.")
   (bidi-class :initarg :bidi-class
               ;; will be defaulted later, see
               ;; SET-DEFAULT-BIDI-CLASSES
               :initform nil
               :accessor bidi-class*
               :type symbol
               :documentation "The Bidi class of the character - a
property symbol.")
   (bidi-mirroring-glyph :initform nil
                         :accessor bidi-mirroring-glyph*
                         :type (or fixnum null)
                         :documentation "The code point of the mirror
image of the character, if there is one.")
   (binary-props :initarg :binary-props
                 :initform nil
                 :accessor binary-props*
                 :type list
                 :documentation "A list of property symbols denoting
the binary properties of the character.")
   (combining-class :initarg :combining-class
                    ;; the default combining class
                    :initform 0
                    :reader combining-class*
                    :type fixnum
                    :documentation "The combining class of the
character - an integer.")
   (numeric-type :initarg :numeric-type
                 :initform nil
                 :reader numeric-type*
                 :type symbol
                 :documentation "The numeric type \(one of
\"Decimal\", \"Digit\", or \"Numeric\") of the character if it has one
- a property symbol.")
   (numeric-value :initarg :numeric-value
                  :initform nil
                  :reader numeric-value*
                  :type (or rational null)
                  :documentation "The numeric value of the character
if it has one - a Lisp rational.")
   (uppercase-mapping :initarg :uppercase-mapping
                      :initform nil
                      :reader uppercase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple uppercase mapping of
the character \(as a code point) if explicitly specified.")
   (lowercase-mapping :initarg :lowercase-mapping
                      :initform nil
                      :reader lowercase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple lowercase mapping of
the character \(as a code point) if explicitly specified.")
   (titlecase-mapping :initarg :titlecase-mapping
                      :initform nil
                      :reader titlecase-mapping*
                      :type (or fixnum null)
                      :documentation "The simple titlecase mapping of
the character \(as a code point) if explicitly specified."))
  (:documentation "A CHAR-INFO object is a datastructure which is used
to \(temporarily) hold the information about one character as gathered
from parsing the Unicode data files - see the code in read.lisp."))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/read.lisp,v 1.34 2012-05-04 21:17:45 edi Exp $

(defmacro with-unicode-file ((file-name contents-name) &body body)
  "Utility macro to parse a file which is formatted as described in
<http://unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format>.  The
file named FILE-NAME is searched for in the directory \"data/\"
relative to this source file.  The code then iterates through the file
and executes BODY for each non-comment line binding contents to the
list of fields in the line"
  `(let ((pathname (merge-pathnames ,file-name (merge-pathnames "data/" *this-file*))))
       (when *compile-verbose*
         (format t "~&;;; Parsing Unicode file ~A" (file-namestring pathname))
         (force-output))
       (with-open-file (binary-in pathname :element-type 'flex:octet)
         ;; Unicode data files must be read as UTF-8
         (let ((in (flex:make-flexi-stream binary-in :external-format '(:utf-8 :eol-style :lf))))
           (loop
            (flet ((get-line-contents ()
                     (let ((line (or (read-line in nil) (return))))
                       (and (not (ppcre:scan "^\\s*(?:#|$)" line))
                            (ppcre:split "\\s*(#.*)?$|\\s*;\\s*" line :limit most-positive-fixnum)))))
              (let ((,contents-name (get-line-contents)))
                (when ,contents-name
                  ,@body))))))))

(defmacro with-unicode-codepoint-file (((&rest bindings) file-name &optional two-line-ranges) &body body)
  "Utility macro to parse a file which is formatted as described in
<http://unicode.org/Public/UNIDATA/UCD.html#UCD_File_Format>.  The
file named FILE-NAME is searched for in the directory \"data/\"
relative to this source file.  The code then iterates through the file
and executes BODY for each non-comment line binding the variables in
BINDINGS to the parsed fields of the line.  For the details of
BINDINGS see the EXTRACT-FOO functions in util.lisp or the usage of
this macro below.  If TWO-LINE-RANGES is true, then the macro expects
a file like \"UnicodeData.txt\" where ranges aren't denoted as usual
but rather using <..., First> and <..., Last>."
  (let ((variables (extract-variables bindings))
        (types (extract-types bindings)))
    `(with-unicode-file (,file-name contents)
      (destructuring-bind ,variables
          (parse-one-line contents ',types (list ,@(extract-defaults bindings)))
        ,@(when two-line-ranges
            `((when (ppcre:scan "^<.*, First>$" ,(second variables))
                (let ((range-end (first (parse-one-line (list (first (get-line-contents)))))))
                  (setq ,(first variables) (cons ,(first variables) range-end))))))
        ,@body))))

(defmacro with-code-point-range ((var range) &body body)
  "Utility macro which executes BODY with VAR bound to each code point
in RANGE in turn.  VAR can either be an integer \(for one code point)
or a cons of two integers \(for an inclusive range)."
  (with-rebinding (range)
    `(flet ((thunk (,var) ,@body))
       (cond ((atom ,range) (thunk ,range))
             (t (loop for point from (car ,range) to (cdr ,range)
                      do (thunk point)))))))

(defun read-character-data ()
  "Parses the file \"UnicodeData.txt\" and generates one CHAR-INFO
entry per code point which is stored in *CHAR-DATABASE*."
  ;; by definition, we'll never see this property in the file, so we
  ;; have to add it to *GENERAL-CATEGORIES* explicitly
  (setq *general-categories* (list '#.(property-symbol "Cn")))
  (with-unicode-codepoint-file ((code-point-range
                       name
                       (general-category symbol)
                       (combining-class integer)
                       (bidi-class symbol)
                       ;; decomposition mapping, ignored for now
                       _
                       (decimal-digit integer nil)
                       (digit integer nil)
                       (numeric rational nil)
                       (bidi-mirrored boolean)
                       (unicode1-name string nil)
                       ;; ISO comment, ignored
                       _
                       (uppercase-mapping hex nil)
                       (lowercase-mapping hex nil)
                       (titlecase-mapping hex nil))
                      "UnicodeData.txt" t)
    (pushnew general-category *general-categories* :test #'eq)
    (pushnew bidi-class *bidi-classes* :test #'eq)
    ;; if the name starts with #\<, it's not really a name but denotes
    ;; a range - some of these names (CJK unified ideographs and
    ;; Hangul syllables) will be computed later, the others are NIL
    (when (char= (char name 0) #\<)
      (setq name nil))
    (with-code-point-range (code-point code-point-range)
      (setf (aref *char-database* code-point)
            (make-instance 'char-info
                           :code-point code-point
                           :name name
                           :general-category general-category
                           :combining-class combining-class
                           :bidi-class bidi-class
                           :numeric-type (cond (decimal-digit '#.(property-symbol "Decimal"))
                                               (digit '#.(property-symbol "Digit"))
                                               (numeric '#.(property-symbol "Numeric")))
                           :numeric-value numeric
                           :binary-props (and bidi-mirrored
                                              (list '#.(property-symbol "BidiMirrored")))
                           :unicode1-name unicode1-name
                           :uppercase-mapping uppercase-mapping
                           :lowercase-mapping lowercase-mapping
                           :titlecase-mapping titlecase-mapping)))))

(defun read-scripts ()
  "Parses the file \"Scripts.txt\" and adds the information about the
script to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (script symbol)) "Scripts.txt")
    (pushnew script *scripts* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (script* char-info) script))))))

(defun read-code-blocks ()
  "Parses the file \"Blocks.txt\" and adds the information about the
code block to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (code-block symbol)) "Blocks.txt")
    (pushnew code-block *code-blocks* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (code-block* char-info) code-block))))))

(defun read-word-breaks ()
  "Parses the file \"Scripts.txt\" and adds the information about the
script to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (word-break symbol)) "auxiliary/WordBreakProperty.txt")
    ;(pushnew word-break *word-breaks* :test #'eq)
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (word-break* char-info) word-break))))))

(defun read-binary-properties ()
  "Parses the file \"PropList.txt\" and adds information about binary
properties to the corresponding entries in *CHAR-DATABASE*."
  ;; this property was derived from UnicodeData.txt already
  (setq *binary-properties* (list '#.(property-symbol "BidiMirrored")))
  (with-unicode-codepoint-file ((code-point-range (property symbol)) "PropList.txt")
    ;; we don't need this information as we derive it from a code
    ;; point not being mentioned in UnicodeData.txt - see also the
    ;; initform for GENERAL-CATEGORY in the definition of CHAR-INFO
    (unless (eq property '#.(property-symbol "NoncharacterCodePoint"))
      (pushnew property *binary-properties* :test #'eq)
      (with-code-point-range (code-point code-point-range)
        (let ((char-info (aref *char-database* code-point)))
          (unless char-info
            ;; this file actually contains some information for
            ;; unassigned (but reserved) code points, like e.g. #xfff0
            (setf char-info (make-instance 'char-info :code-point code-point)
                  (aref *char-database* code-point) char-info))
          (push property (binary-props* char-info)))))))

(defun read-derived-age ()
  "Parses the file \"DerivedAge.txt\" and adds information about the
\"age\" to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (age age)) "DerivedAge.txt")
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (age* char-info) age))))))

(defun read-mirroring-glyphs ()
  "Parses the file \"BidiMirroring.txt\" and adds information about
mirroring glyphs to the corresponding entries in *CHAR-DATABASE*."
  (with-unicode-codepoint-file ((code-point-range (mirroring-glyph hex)) "BidiMirroring.txt")
    (with-code-point-range (code-point code-point-range)
      (let ((char-info (aref *char-database* code-point)))
        (when char-info
          (setf (bidi-mirroring-glyph* char-info) mirroring-glyph))))))

(defun read-jamo ()
  "Parses the file \"Jamo.txt\" and stores information about Jamo
short names in the *JAMO-SHORT-NAMES* hash table.  This information is
later used to compute Hangul syllable names."
  (clrhash *jamo-short-names*)
  (with-unicode-codepoint-file ((code-point-range (short-name string "")) "Jamo.txt")
    (with-code-point-range (code-point code-point-range)
      (setf (gethash code-point *jamo-short-names*) short-name))))

(defun read-property-aliases ()
  "Parses the file \"PropertyAliases.txt\" and stores information about
aliases for properties in the *PROPERTY-ALIASES* hash table.  This
information is used to get properties by any alias."
  (clrhash *property-aliases*)
  (with-unicode-file ("PropertyAliases.txt" contents)
    (destructuring-bind (short-name long-name &rest additional-names) contents
      (let ((symb (property-symbol long-name))
            (long-name (string-upcase (canonicalize-name long-name)))
            (short-name (string-upcase (canonicalize-name short-name))))
        (setf (gethash long-name *property-aliases*) symb
              (gethash short-name *property-aliases*) symb)
        (loop for alt-name in additional-names
          :do (setf (gethash (string-upcase (canonicalize-name alt-name)) *property-aliases*) symb))))))

(defun default-bidi-class (char-info)
  "Returns the default Bidi class for the character described by the
CHAR-INFO object CHAR-INFO.  The default is computed as explained in
<http://unicode.org/Public/UNIDATA/extracted/DerivedBidiClass.txt>."
  (let ((code-point (code-point char-info)))
    (cond ((and (or (<= #x0600 code-point #x07BF)
                    (<= #xFB50 code-point #xFDFF)
                    (<= #xFE70 code-point #xFEFF))
                (not (find '#.(property-symbol "NoncharacterCodePoint")
                           (binary-props* code-point))))
           '#.(property-symbol "AL"))
          ((or (<= #x0590 code-point #x05FF)
               (<= #x07C0 code-point #x08ff)
               (<= #xFB1D code-point #xFB4F)
               (<= #x10800 code-point #x10FFF))
           '#.(property-symbol "R"))
          (t '#.(property-symbol "L")))))

(defun set-default-bidi-classes ()
  "Loops through all assigned characters in *CHAR-DATABASE* and
defaults their Bidi class if it wasn't set already."
  (loop for char-info across *char-database*
        when (and char-info (not (bidi-class* char-info)))
        do (let ((default-bidi-class (default-bidi-class char-info)))
             (pushnew default-bidi-class *bidi-classes* :test #'eq)
             (setf (bidi-class* char-info) default-bidi-class))))

(defun fill-database ()
  "Initializes all relevant datastructures and parses all Unicode data
files in the \"data/\" directory to build up enough information in
memory \(specifically the *CHAR-DATABASE* array) to write the missing
source code files for CL-UNICODE."
  (setq *char-database* (make-empty-char-database)
        *general-categories* nil
        *scripts* nil
        *code-blocks* nil
        *binary-properties* nil
        *bidi-classes* nil)
  (initialize-property-symbols)
  (read-character-data)
  (read-scripts)
  (read-code-blocks)
  (read-word-breaks)
  (read-binary-properties)
  (read-derived-age)
  (read-mirroring-glyphs)
  (read-jamo)
  (read-property-aliases)
  (set-default-bidi-classes))

(defun build-name-mappings ()
  "Initializes and fills the hash tables which map code points to
\(Unicode 1.0) names and vice versa using the information in
*CHAR-DATABASE*."
  (clrhash *names-to-code-points*)
  (clrhash *unicode1-names-to-code-points*)
  (clrhash *code-points-to-names*)
  (clrhash *code-points-to-unicode1-names*)
  (loop for char-info across *char-database*
        for name = (and char-info (name char-info))
        for unicode1-name = (and char-info (unicode1-name char-info))
        for code-point = (and char-info (code-point char-info))
        when name
        do (setf (gethash code-point *code-points-to-names*) name
                 (gethash (canonicalize-name name) *names-to-code-points*) code-point)
        when unicode1-name
        do (setf (gethash code-point *code-points-to-unicode1-names*) unicode1-name
                 (gethash (canonicalize-name unicode1-name) *unicode1-names-to-code-points*) code-point)))

(defun build-case-mapping ()
  "Initializes and filles the *CASE-MAPPINGS* hash table from
*CHAR-DATABASE*."
  (clrhash *case-mappings*)
  (loop for char-info across *char-database*
        for mappings = (and char-info
                            (list (uppercase-mapping* char-info)
                                  (lowercase-mapping* char-info)
                                  (titlecase-mapping* char-info)))
        when (and mappings (some #'identity mappings))
        do (setf (gethash (code-point char-info) *case-mappings*) mappings)))

(defun build-data-structures ()
  "One function to combine the complete process of parsing all Unicode
data files and building the corresponding Lisp datastructures in
memory."
  (fill-database)
  (when *compile-verbose*
    (format t "~&;;; Building hash tables")
    (force-output))
  (build-name-mappings)
  (build-case-mapping))

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/cl-unicode/build/dump.lisp,v 1.39 2012-05-04 21:17:45 edi Exp $

(defun build-range-list (reader test)
  "Loops through all non-NIL elements of *CHAR-DATABASE*, looks at
each element using the reader READER and builds an alist of the form
\((X1 . A1) (X2 . A2)) where the Xi are code points and the Ai are the
corresponding attributes as returned by READER.  The interesting part
is that the Xi can also be code point ranges like \(Y1 . Y2) meaning
that all code points from Y1 to \(including) Y2 have the same
attribute Ai and the function tries to make the alist as short as
possible, i.e. to partition it into as few ranges as possible.
Whether two attributes are the same is determined by the test function
TEST.  The resulting list is sorted."
  (let (range-list
        (last-attribute (funcall reader (aref *char-database* 0)))
        (last-code-point 0)
        (code-point 0))
    (flet ((add ()
             "Adds the range from LAST-CODE-POINT to \(excluding)
CODE-POINT with the attribute LAST-ATTRIBUTE to the result."
             (push (cons (cons last-code-point (1- code-point))
                         last-attribute)
                   range-list)))
      (loop
       (incf code-point)
       (when (= code-point #.(1- +code-point-limit+))
         (add)
         (return))
       (let* ((char-info (aref *char-database* code-point))
              (attribute (and char-info (funcall reader char-info))))
         (unless (funcall test attribute last-attribute)
           (add)
           (setq last-attribute attribute
                 last-code-point code-point)))))
    (nreverse range-list)))

(defun split-range-list (range-list)
  "Recursively splits a range list as returned by BUILD-RANGE-LIST in
the middle and thus converts it into a binary search tree which can be
used by the TREE-LOOKUP function."
  (let ((length (length range-list)))
    (cond ((zerop length) nil)
          (t (let ((middle (round (1- length) 2)))
               (list (nth middle range-list)
                     (split-range-list (subseq range-list 0 middle))
                     (split-range-list (subseq range-list (1+ middle)))))))))

(defun build-tree (reader &optional (test #'eq))
  "Uses BUILD-RANGE-LIST and SPLIT-RANGE-LIST to build a binary search
tree for READER which is one of the readers of the CHAR-INFO class.
Attributes are compared with TEST."
  (split-range-list (build-range-list reader test)))

(defun dump-method (name reader stream &optional (test #'eq test-provided-p))
  "Writes a method definition for a unary method called NAME
specialized for integer \(code points) to the stream STREAM which
returns a value equivalent to

  \(APPLY READER \(AREF *CHAR-DATABASE* <code-point>))

but uses compact binary search trees instead of the *CHAR-DATABASE*
array.  TEST is used by BUILD-TREE to decide whether two adjacent
characters have the same attribute.  If TEST isn't provided, it is
assumed that the attribute is a property symbol and the method will
return two values - the symbol and the canonical name of the symbol."
  (let ((definition (if test-provided-p
                      `(defmethod ,name ((code-point integer))
                         (tree-lookup code-point ',(build-tree reader test)))
                      `(defmethod ,name ((code-point integer))
                         (let ((symbol (tree-lookup code-point ',(build-tree reader))))
                           (values (property-name symbol) symbol))))))
    (print definition stream)))

(defmacro with-output-to-source-file ((stream relative-path &key no-header-p) &body body)
  "Executes BODY with STREAM bound to an output file stream which
writes to the file denoted by RELATIVE-PATH - a path relative to the
location of this source file.  Writes a Lisp header to the files
unless NO-HEADER-P is true."
  `(let ((pathname (merge-pathnames ,relative-path
                                    (make-pathname :name nil
                                                   :type nil
                                                   :version nil
                                                   :defaults *this-file*))))
     (when *compile-verbose*
       (format t "~&;;; Writing source file ~A" (file-namestring pathname))
       (force-output))
     (with-open-file (,stream pathname
                              :direction :output
                              :if-exists :supersede)
       (with-standard-io-syntax ()
         (let ((*package* (find-package :cl-unicode))
               #+clisp
               (*print-readably* nil)) ;; clisp produces rubbish otherwise
           ,@(unless no-header-p
               '((format out ";;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-UNICODE; Base: 10 -*-~%")))
           (format out ";;; This file was auto-generated by the BUILD-CL-UNICODE system~2%")
           ,@(unless no-header-p
               '((print '(in-package :cl-unicode) out)))
           ,@body)))))

(defun dump-methods ()
  "Dumps several methods to the CL-UNICODE source file methods.lisp
using DUMP-METHOD."
  (with-output-to-source-file (out "methods.lisp") ;(out "../methods.lisp")
    (dump-method 'script 'script* out)
    (dump-method 'code-block 'code-block* out)
    (dump-method 'word-break 'word-break* out)
    (dump-method 'age 'age* out #'equal)
    (dump-method 'general-category 'general-category* out)
    (dump-method 'bidi-class 'bidi-class* out)
    (dump-method 'numeric-type 'numeric-type* out)
    (dump-method 'numeric-value 'numeric-value* out #'eql)
    (dump-method 'combining-class 'combining-class* out #'eql)
    (dump-method 'bidi-mirroring-glyph% 'bidi-mirroring-glyph* out #'eql)
    (dump-method 'binary-props 'binary-props* out #'equal)))

(defun dump-hash-table (hash-table-name stream)
  "Writes code to the STREAM which reinitializes the hash table
contained in the global special variable named HASH-TABLE-NAME to its
current state.  It is assumed that all keys and values of the hash
table can be printed readably."
  (print `(clrhash ,hash-table-name) stream)
  (let ((key-value-alist
         (loop for key being the hash-keys of (symbol-value hash-table-name)
               using (hash-value value)
               when value
               collect (cons key value))))
    (print `(loop for (key . value) in ',key-value-alist
                  do (setf (gethash key ,hash-table-name) value))
           stream)))

(defun dump-hash-tables ()
  "Dumps several hash tables to the CL-UNICODE source file
hash-tables.lisp using DUMP-HASH-TABLE."
  (with-output-to-source-file (out "hash-tables.lisp") ;(out "../hash-tables.lisp")
    (dump-hash-table '*canonical-names* out)
    (dump-hash-table '*names-to-code-points* out)
    (dump-hash-table '*code-points-to-names* out)
    (dump-hash-table '*unicode1-names-to-code-points* out)
    (dump-hash-table '*code-points-to-unicode1-names* out)
    (dump-hash-table '*case-mappings* out)
    (dump-hash-table '*jamo-short-names* out)
    (dump-hash-table '*property-aliases* out)
    ;; finally add code which adds the computed Hangul syllable names
    ;; at load time
    (print '(add-hangul-names) out)))

(defun dump-list (list-name stream)
  "Writes code to the STREAM which reinitializes the list contained in
the global special variable named LIST-NAME to its current state.  It
is assumed that all elements of the list can be printed readably."
  (print `(setq ,list-name ',(symbol-value list-name)) stream))

(defun dump-lists ()
  "Dumps several list to the CL-UNICODE source file lists.lisp using
DUMP-LIST."
  (with-output-to-source-file (out "lists.lisp")
    (dump-list '*general-categories* out)
    (dump-list '*scripts* out)
    (dump-list '*code-blocks* out)
    (dump-list '*binary-properties* out)
    (dump-list '*bidi-classes* out)))

(defun dump-derived-tests ()
  "Parses the Unicode data file \"DerivedCoreProperties.txt\" \(which
is not used in read.lisp) and uses it to create a file
\"derived-properties\" which will be used by CL-UNICODE-TEST."
  (with-output-to-source-file (out (make-pathname :name "derived-properties"
                                                  :type nil
                                                  :directory '(:relative "test"))
                                   :no-header-p t)
    (let (last-test)
      (labels ((really-add-test (test)
                 "Writes the test designator from ADD-TEST in a
\"delayed\" fashion to make sure that the previous test \(which hasn't
been written to disk yet) doesn't contradict the current test.  This
is necessary because the file we're parsing contains several adjacent
ranges."
                 (when last-test
                   (unless (= (first last-test) (first test))
                     (print last-test out)))
                 (setq last-test test))
               (add-test (code-point property &optional (successp t))
                 "Writes a test designator \(used by the function
CL-UNICODE-TEST::PROPERTY-TESTS) for a check whether CODE-POINT has
the property PROPERTY to the stream OUT, but only does this if
CODE-POINT is below +CODE-POINT-LIMIT+.  Tests for the inverse if
SUCCESSP is NIL.  The test designator isn't actually written to the
stream, though, but handed over to REALLY-ADD-TEST."
                 (when (< code-point +code-point-limit+)
                   (really-add-test `(,code-point ,property ,successp)))))
        (with-unicode-codepoint-file ((code-point-range property) "DerivedCoreProperties.txt")
          (cond ((atom code-point-range)
                 (add-test code-point-range property)
                 (add-test (1+ code-point-range) property nil))
                (t
                 (add-test (car code-point-range) property)
                 (add-test (cdr code-point-range) property)
                 (add-test (1+ (cdr code-point-range)) property nil))))       
        (print last-test out)))))

(defun dump-data-structures ()
  "Dumps all the information contained in *CHAR-DATABASE* and the
related hash tables and lists to the corresponding Lisp and test
source files."
  (dump-methods)
  (dump-hash-tables)
  (dump-lists)
  (dump-derived-tests)
  (setq *char-database* nil))

(defun create-source-files ()
  "Combines BUILD-DATA-STRUCTURES and DUMP-DATA-STRUCTURES to create
the \"missing\" CL-UNICODE source files."
  (build-data-structures)
  (dump-data-structures)
  (setq *char-database* nil))

