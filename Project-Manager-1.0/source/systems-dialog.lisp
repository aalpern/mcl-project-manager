;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T - M A N A G E R
;;
;; file: 	systems-dialog.lisp
;; author: 	David B. Lamkins
;; created: 	12/2/1995
;;
;;	Provides a dialog listing all available systems, for those
;;	who want to save space in the menubar. Set *project-manager-use-menu*
;;	to nil before calling setup-project-menu to use it. Adds a "Systems"
;;	item to the "Tools" menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; dbl = David B. Lamkins
;; ala = Adam L. Alpern
;;
;; 04/28/96	ala	- Duh. Project manager requires UI-lib. No need for conditionals.
;;			- new all-systems-dialog. More compact, and resizable.
;; 04/05/96	ala	- use UI-lib is it's installed at compile time
;; 04/02/96 	ala	- updated "Options" action for new prefs dialog
;;	 	ala	- renamed to systems-dialog.lisp
;; 12/02/95	dbl	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *systems-dialog* nil)

(defparameter *systems-dialog-default-position* #@(2 40))
(defparameter *systems-dialog-default-size* 	#@(239 109))

(defclass all-systems-dialog (3d-dialog) ()
  (:default-initargs
    :window-type 	:document
    :window-title 	"Systems"
    :view-font		'("Geneva" 9 :plain)
    :view-size 		*systems-dialog-default-size*
    :view-position	*systems-dialog-default-position*
    :close-box-p 	t
    :grow-icon-p 	t))

(defun all-systems-dialog-parts (w)
  (declare (ignore w))
  (list      
   (make-dialog-item '3d-button-dialog-item
                     #@(160 70)
                     #@(72 18)
                     "Open"
                     #'(lambda (item)
                         (let ((table (find-named-sibling item :system-list)))
                           (dolist (sysname
                                    (mapcar #'(lambda (cell)
                                                (cell-contents table cell))
                                            (selected-cells table)))
                             (let ((old-window
                                    (find-window (string sysname) 
                                                 'project-manager-window)))
                               (if old-window
                                 (window-select old-window)
                                 (manage-system sysname))))))
                     :view-nick-name :open-button
                     :Default-Button t
                     :square t :inset nil)
   (make-dialog-item '3d-button-dialog-item
                     #@(157 46)
                     #@(79 20)
                     "Load"
                     #'(lambda (item)
                         (let ((table (find-named-sibling item :system-list)))
                           (dolist (sysname
                                    (mapcar #'(lambda (cell) 
                                                (cell-contents table cell))
                                            (selected-cells table)))
                             (eval-enqueue `(make::operate-on-system 
                                             ,sysname
                                             :load
                                             ))
                             )))
                     :square t
                     :inset t
                     :view-nick-name :load-button)
   (make-dialog-item '3d-button-dialog-item
                     #@(157 25)
                     #@(79 20)
                     "Update List"
                     #'(lambda (item)
                         (set-table-sequence (find-named-sibling item :system-list)
                                             (systems)))
                     :square t :inset t
                     :view-nick-name :update-button)
   (make-dialog-item '3d-button-dialog-item
                     #@(157 4)
                     #@(79 20)
                     "Options…"
                     #'(lambda (item)
                         (declare (ignore item))
                         (modal-dialog (make-instance
                                         'preference-dialog
                                         :sheets
                                         (projman-preference-sheets))))
                     :square t :inset t
                     :view-nick-name :options-button)
   (make-dialog-item '3d-sequence-dialog-item
                     #@(6 6)
                     #@(147 97) 
                     ""
                     #'(lambda (item)
                         (let ((default-button (default-button
                                                 (view-container item))))
                           (if (double-click-p)
                             (press-button default-button)
                             (if (selected-cells item)
                               (dialog-item-enable default-button)
                               (dialog-item-disable default-button)))))
                     :view-nick-name :system-list
                     :cell-size #@(133 12)
                     :selection-type :single
                     :table-hscrollp nil
                     :table-vscrollp t
                     :table-sequence (systems))))

(defmethod initialize-instance :after ((w all-systems-dialog) &rest args)
  (declare (ignore args))
  (apply #'add-subviews w (all-systems-dialog-parts w)))

(defmethod set-view-size :after ((w all-systems-dialog) h &optional v)
  (declare (ignore h v))  
  (set-view-size (view-named :system-list w)
                 (make-point (- (view-size-h w) 96)
                             (- (view-size-v w) 12)))    
  (set-view-position-h (view-named :update-button w) 
                       (- (view-size-h w)
                          (view-size-h (view-named :update-button w) )
                          6))
    (set-view-position-h (view-named :load-button w) 
                       (- (view-size-h w)
                          (view-size-h (view-named :load-button w) )
                          6))
  (set-view-position-h (view-named :options-button w) 
                       (- (view-size-h w)
                          (view-size-h (view-named :options-button w) )
                          6))
  (set-view-position-h (view-named :open-button w) 
                       (- (view-size-h w)
                          (view-size-h (view-named :open-button w) )
                          10)))

(defun systems-dialog ()
  (if (and *systems-dialog* (wptr *systems-dialog*))
    (window-select *systems-dialog*)
    (setq *systems-dialog* (make-instance 'all-systems-dialog))))