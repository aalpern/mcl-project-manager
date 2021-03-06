;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T - M A N A G E R
;;
;; file: 	projman-prefs-sheets.lisp
;; author: 	Adam Alpern
;; created: 	2/19/1996
;;
;;	Preference sheets for the Defsystem Project Manager.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 04/05/96	- completed PATHS sheet
;;		- ui-lib conditionalizations. Don't use it if not
;;		  there at compile time.
;; 04/02/96	- polished up, made it functional
;;		- completed MISC,DEFSYSTEM sheets
;; 02/19/96	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-check-box-check (check-box-dialog-item check)
  (if check
    (check-box-check check-box-dialog-item)
    (check-box-uncheck check-box-dialog-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sheet functions:  DEFSYSTEM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun projman-defsystem-default-function (sheet)
  (declare (ignore sheet)))

(defun projman-defsystem-update-function (sheet)
  (set-check-box-check (sheet-view-named :load-source-instead-of-binary sheet)
                       make::*load-source-instead-of-binary*)
  (set-check-box-check (sheet-view-named :load-source-if-no-binary sheet)
                       make::*load-source-if-no-binary*)
  (set-check-box-check (sheet-view-named :compile-during-load sheet)
                       make:*compile-during-load*)
  (set-check-box-check (sheet-view-named :minimal-load sheet)
                       make:*minimal-load*)
  (set-check-box-check (sheet-view-named :verbose sheet)
                       make::*oos-verbose*)
  (set-check-box-check (sheet-view-named :operations-propagate-to-subsystems sheet)
                       make::*operations-propagate-to-subsystems*)
  (set-check-box-check (sheet-view-named :files-missing-is-an-error sheet)
                       make:*files-missing-is-an-error*)
  (set-check-box-check (sheet-view-named :bother-user-if-no-binary sheet)
                       make::*bother-user-if-no-binary*))

(defun projman-defsystem-set-function (sheet)
  (setf make::*load-source-instead-of-binary*
        (check-box-checked-p (sheet-view-named :load-source-instead-of-binary sheet))
        make::*load-source-if-no-binary*
        (check-box-checked-p (sheet-view-named :load-source-if-no-binary sheet))
        make:*compile-during-load*
        (check-box-checked-p (sheet-view-named :compile-during-load sheet))
        make:*minimal-load*
        (check-box-checked-p (sheet-view-named :minimal-load sheet))
        make::*oos-verbose*
        (check-box-checked-p (sheet-view-named :verbose sheet))
        make::*operations-propagate-to-subsystems*
        (check-box-checked-p (sheet-view-named :operations-propagate-to-subsystems sheet))
        make:*files-missing-is-an-error*
        (check-box-checked-p (sheet-view-named :files-missing-is-an-error sheet))
        make::*bother-user-if-no-binary*
        (check-box-checked-p (sheet-view-named :bother-user-if-no-binary sheet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sheet functions:  MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun projman-misc-default-function (sheet)
  (set-check-box-check (sheet-view-named :color-p sheet) t))

(defun projman-misc-set-function (sheet)
  (setf *project-manager-color-p* (check-box-checked-p
                                   (sheet-view-named :color-p sheet))))

(defun projman-misc-update-function (sheet)
  (set-check-box-check (sheet-view-named :color-p sheet)
                       *project-manager-color-p*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sheet functions:  PATHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun projman-paths-update-function (sheet)
  (set-dialog-item-text (sheet-view-named :central-registry sheet)
                        (format nil "~s" make:*central-registry*)))

(defun projman-paths-set-function (sheet)
  (setf make:*central-registry*
        (read-from-string (dialog-item-text
                           (sheet-view-named :central-registry sheet)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; preference sheets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun projman-preference-sheets ()
  (list

   (make-instance 'preference-sheet
     :name "Paths"
     :set-function 	'projman-paths-set-function
     :default-function 	nil
     :update-function 	'projman-paths-update-function
     :views (list (make-dialog-item
                   #+ui-lib '3d-editable-text-dialog-item
                   #-ui-lib 'editable-text-dialog-item
                   #@(145 57) #@(251 12)
                   ""
                   'nil
                   :view-nick-name :central-registry
                   :view-font '("Geneva" 9 :srcor :plain (:color-index 0))
                   :allow-returns  nil
                   :draw-outline -2)
                  (make-dialog-item
                   #+ui-lib '3d-button-dialog-item
                   #-ui-lib  'button-dialog-item
                   #@(142 77)
                   #@(66 21)
                   "Choose�"
                   #'(lambda (item)
                       (set-dialog-item-text
                        (find-named-sibling item :central-registry)
                        (format nil
                                "~s"
                                (edit-pathnames
                                 (if (listp make:*central-registry*)
                                   make:*central-registry*
                                   (list make:*central-registry*))))))
                   :view-font '("Geneva" 9 :srcor :plain (:color-index 0))
                   #+ui-lib :inset #+ui-lib t
                   #+ui-lib :frame #+ui-lib t
                   #+ui-lib :square #+ui-lib t)
                  (make-dialog-item
                   'static-text-dialog-item
                   #@(142 37)  #@(111 12)
                   "Central Registry"
                   'nil
                   :view-font '("Geneva" 9 :srcor :italic (:color-index 0)))))

   (make-instance 'preference-sheet
     :name "Defsystem"
     :set-function 	'projman-Defsystem-set-function
     :default-function 	'projman-Defsystem-default-function
     :update-function 	'projman-Defsystem-update-function
     :views (list (make-dialog-item
                   'static-text-dialog-item
                   #@(148 36) #@(216 14)
                   "Loading Options" 'nil
                   :view-font  '("Geneva" 9 :srcor :italic)
                   )
                  (make-dialog-item
                   'static-text-dialog-item
                   #@(148 162)
                   #@(216 14)
                   "Errors/Reporting"
                   'nil
                   :view-font  '("Geneva" 9 :srcor :italic)
                   )
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 197)
                   #@(162 16)
                   "Bother user if no binary"
                   'nil
                   :view-nick-name
                   :bother-user-if-no-binary
                   :view-font  '("Geneva" 9 :srcor :plain ))
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 104)
                   #@(84 16)
                   "Minimal load"
                   'nil
                   :view-nick-name
                   :minimal-load
                   :view-font
                   '("Geneva" 9 :srcor :plain ))
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 121)
                   #@(59 16)
                   "Verbose"
                   'nil
                   :view-nick-name
                   :verbose
                   :view-font
                   '("Geneva" 9 :srcor :plain )
                   :check-box-checked-p
                   t)
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 87)
                   #@(116 16)
                   "Compile during load"
                   'nil
                   :view-nick-name
                   :compile-during-load
                   :view-font
                   '("Geneva" 9 :srcor :plain )
                   :check-box-checked-p
                   t)
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 53)
                   #@(162 16)
                   "Load source instead of binary"
                   'nil
                   :view-nick-name
                   :load-source-instead-of-binary
                   :view-font
                   '("Geneva" 9 :srcor :plain ))
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 70)
                   #@(175 16)
                   "Load source if no binary"
                   'nil
                   :view-nick-name
                   :load-source-if-no-binary
                   :view-font
                   '("Geneva" 9 :srcor :plain ))
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 138)
                   #@(197 16)
                   "Operations propagate to subsystems"
                   'nil
                   :view-nick-name
                   :operations-propagate-to-subsystems
                   :view-font
                   '("Geneva" 9 :srcor :plain )
                   :check-box-checked-p
                   t)
                  (make-dialog-item
                   'check-box-dialog-item
                   #@(155 180)
                   #@(162 16)
                   "Files missing is an error"
                   'nil
                   :view-nick-name
                   :files-missing-is-an-error
                   :view-font  '("Geneva" 9 :srcor )
                   :check-box-checked-p
                   t)))

   (make-instance 'preference-sheet
     :name "Misc"
     :set-function 	'projman-Misc-set-function
     :default-function 	'projman-Misc-default-function
     :update-function 	'projman-Misc-update-function
     :views (list (make-dialog-item
                   'check-box-dialog-item
                   #@(155 53)
                   #@(162 16)
                   "Color-P"
                   'nil
                   :view-nick-name 	:color-p
                   :view-font		'("Geneva" 9 :srcor :plain )
                   :check-box-checked-p *project-manager-color-p*)))

   ))

#|
(modal-dialog
 (make-instance 'preference-dialog
   :sheets  (projman-preference-sheets)))
|#
