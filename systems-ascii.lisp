(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf) 
)

(defparameter *this-file* #.(or *compile-file-pathname* *load-pathname*))
(defparameter *working-directory* (make-pathname :defaults *this-file* :type nil :name nil :version nil))
(defparameter *dependencies* (merge-pathnames "depends/" *working-directory*))

(dolist (p '(*this-file* *working-directory* *dependencies*))
  (format t "; ~a ~s~%" p (symbol-value p)))

(defparameter *systems* ()) ; retain loaded system keywords

(defun loader (name path url)
  (declare (ignore url))
  (unless (find name *systems*)
    (let ((path (if (probe-file path) path (merge-pathnames path *dependencies*))))
      (format t "Loading ~s: ~a~%" name path)
      (unless (probe-file path) (error "ASDF system definition file can't be found: ~a" path))
      (require name path)
      (asdf:load-system name)
      (pushnew name *systems*))))

;;(loader :trivial-gray-streams "trivial-gray-streams/trivial-gray-streams-master/trivial-gray-streams.asd" "https://github.com/trivial-gray-streams/trivial-gray-streams")
;;(loader :flexi-streams "flexi-streams/flexi-streams-1.0.19/flexi-streams.asd" "http://weitz.de/files/flexi-streams.tar.gz") ; needed for cl-unicode
;;(loader :cl-ppcre "cl-ppcre/cl-ppcre-2.1.1/cl-ppcre.asd" "https://github.com/edicl/cl-ppcre")
;;(loader :cl-unicode "cl-unicode-master/cl-unicode.asd" "https://github.com/edicl/cl-unicode") ; needed by montezuma
(loader :textdefrag "../TextDefrag.asd" "")

