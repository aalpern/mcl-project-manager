;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T  M A N A G E R
;;
;; file: 	resources.lisp
;; author: 	Adam Alpern
;; created: 	6/8/1995
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 04/17/96	- added *pm-subsystem-open-icon*
;; 04/02/96	- added *pm-open-folder-icon*
;; 01/22/96	- new subsystem icon
;; 12/10/95	- removed unused icons
;; 6/8/1995	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun init-projman-resources ()
  (defparameter *pm-system-icon* 		(load-color-icon 2000))
  (defparameter *pm-folder-icon* 		(load-color-icon 2001))
  (defparameter *pm-open-folder-icon* 		(load-color-icon 3000))
  (defparameter *pm-file-icon* 	 		(load-color-icon 2002))
  (defparameter *pm-missing-icon*  		(load-color-icon 2003))
  (defparameter *pm-lisp-icon* 	 		(load-color-icon 2004))
  (defparameter *pm-fasl-icon* 	 		(load-color-icon 2005))
  (defparameter *pm-text-icon*   		(load-color-icon 2007))
  (defparameter *pm-subsystem-icon*   		(load-color-icon 2008))
  (defparameter *pm-subsystem-open-icon*	(load-color-icon 2009))

  (defparameter *projman-icon*	(load-color-icon 128))   ; 18x14

  (defparameter *pm-lock-icon*	(load-color-icon 9000)))

#|
(with-open-resource-file (f *project-manager-rsrc-file*)
  (init-projman-resources))
|#

(defun projman-resources ()
  (with-open-resource-file (f *project-manager-rsrc-file*)
    (let ((res nil)
          (ids '(2000 2001 2002 2003 2004 2005 2007 2008
                 9000 128)))
      (dolist (id ids)
        (push (list (load-color-icon id) "cicn" id) res))
      res)))
