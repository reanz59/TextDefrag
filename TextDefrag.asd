;;;; -*- Mode: LISP; Syntax: ANSI-Common-lisp; Base: 10; Package: ASDF -*-
;;;; The above modeline is required for Genera. Do not change.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :unicode-properties *features*))

(asdf:defsystem :textdefrag
  ;;:name :textdefrag
  :author "Roy Anderson <reanz1959@gmail.com>"
  :version "1.0.0"
  :licence "MIT"
  :description "Reduce defragmentation of words encountered in the extraction of text from PDFs, for example."
  :depends-on (:cl-unicode :cl-ppcre)
  :pathname ""
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
                 (:file "unicode-properties")
                 (:file "defragment")))))


