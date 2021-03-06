(defun defsystem-print-load-file (sysname &optional (stream t))
  (defsystem-print-header sysname stream)
  (let ((system (mk::find-system sysname)))
    (defsystem-print-defsystem system stream)))

(defun defsystem-print-header (sysname stream)
  (format stream ";; -*- mode:lisp; package:cl-user -*-
;;
;; Loader file for ~a
;; Automatically generated on <DATE>
;;
" sysname))

(defun defsystem-print-defsystem (system stream)
  (dolist (c (mk::component-components system))
    (case (mk::component-type c)
      (:file (defsystem-print-file c stream))
      (:module (defsystem-print-module c stream)))))

(defun defsystem-print-module (component stream)
  (dolist (c (mk::component-components component))
    (defsystem-print-file c stream)))

(defun defsystem-print-file (component stream)
  (format stream "(load ~s)~%"
          (mk::component-full-pathname component :source)))
