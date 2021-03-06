;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T   M A N A G E R
;;
;; file: 	manage-system-ccl3.lisp
;; author: 	Adam Alpern
;; created: 	6/11/1995
;;
;;	Puts it all together to display a defsystem and its' modules
;;	and files in a twist-down hierarchy. I'd like a better name
;;	than manage-system.
;;
;;	This version uses MCL 3.0's improved pop-up-menu variations to
;;	add a little menubar at the top of the window.
;;
;;;;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 04/05/96	- added projman-update-window, added "Update Window" item to
;;		  commands menu.
;; 01/22/96	- double-clicking either info bar will collapse the  window
;;		  (leaves the grow-icon though, so you can still resize).
;;		  Double-click again to expand.
;; 11/29/95     Changes from David B. Lamkins incorporated:
;;		- Fixed problem where applying an operation
;;                to a multiple selection would confuse subsequent selection
;;                and highlighting. Changed scroll ranges to more reasonable
;;                sizes.
;;		- added for-selected-components function
;;		- changed scroll-bar-page-size and added
;;		  scroll-bar-scroll-size
;; 11/10/95	- updated my email address.
;; 08/09/95	- MCL 3.0 specific version, with menubar in the window.
;;		- removed E/C/L buttons -> command menu takes care of them.
;; 08/01/95	- adjust field-size of scroller based on subviews
;; 06/11/95	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 3) (safety 0)))

(defclass project-manager-menubar (inspector::bottom-line-mixin view) ())

(defmethod view-default-size ((view project-manager-menubar))
  (let ((container (view-container view)))
    (when container
      (make-point (point-h (view-size container))
                  (+ (ccl::view-font-line-height view) 4)))))

(defmethod install-view-in-window ((view project-manager-menubar) w)
  (declare (ignore w))
  (multiple-value-bind (ff ms)(view-font-codes view)
    (let ((container (view-container view)))
      (when ff
        (do-subviews (sub view)
          (set-view-font-codes sub ff ms)))
      ; sets slots
      (ccl::set-default-size-and-position view container)
      (call-next-method)
      (invalidate-view view))))

; system is the defsystem object
(defun make-project-manager-menubar (system w)
  (make-instance 'project-manager-menubar
    :view-position #@(0 0)
    :view-font '("Geneva" 9 :plain)
    :view-nick-name :menubar
    :view-subviews
    (list
     (system-commands-menu system w))))

#|
(defun for-selected-components (w component-function)
  (let ((cvs (copy-list (selected-components w))))
    (when cvs
      (ccl::with-shown-progress (selected-components-task
                                 "" :denominator (length cvs))
        (dolist (c (sort cvs #'< :key
                         #'(lambda (view)
                             (point-v (view-position view)))))
          (ccl::show-progress selected-components-task :step t
                              :note
                              (princ-to-string (mk::component-name (component c))))
          (funcall component-function c))))))
|#

(defun for-selected-components (w component-function)
  (let ((cvs (copy-list (selected-components w))))
    (when cvs
      (dolist (c (sort cvs #'< :key
                       #'(lambda (view)
                           (point-v (view-position view)))))
        (funcall component-function c)))))

(defun system-commands-menu (system w)
  (make-instance 'pull-down-menu
    :crescent t
    :item-display "Commands"
    :view-position #@(0 0)
    :menu-items
    (list
     (make-instance 'menu-item
       :menu-item-title "Edit Definition"
       :menu-item-action
       #'(lambda ()
           (ed (make::compute-system-path
                (intern (make::component-name system)
                         :keyword)
                (make::component-name system)))))
     (make-instance 'menu-item
       :menu-item-title "Print System Info"
       :menu-item-action #'(lambda ()
                             (make::describe-system
                              (intern (make::component-name system)
                                      :keyword)
                              t)))

     (system-dependencies-menu system)

     (make-instance 'menu-item
       :menu-item-title "-")
     (make-instance 'menu-item
       :menu-item-title "Edit"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (when (typep c 'file-component-view)
                  (ed (path c)))))))
     (make-instance 'menu-item
       :menu-item-title "Compile"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (eval-enqueue `(make::operate-on-component
                                ,(component c)
                                :compile
                                ,(if (or (control-key-p)
                                         (typep c 'file-component-view))
                                   t nil)))))))
     (make-instance 'menu-item
       :menu-item-title "Load"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (eval-enqueue `(make::operate-on-component
                                ,(component c)
                                :load
                                ,(if (or (control-key-p)
                                         (typep c 'file-component-view))
                                   t nil)))))))
     (make-instance 'menu-item
       :menu-item-title "-")
     (make-instance 'menu-item
       :menu-item-title "Delete Binaries"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (eval-enqueue `(make::operate-on-component
                                ,(component c)
                                :delete-binaries
                                ,(if (or (control-key-p)
                                         (typep c 'file-component-view))
                                   t nil)))))))
     (make-instance 'menu-item
       :menu-item-title "Lock file(s)"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (when (probe-file (path c))
                  (lock-file (path c))
                  (setf (locked c) t)
                  (invalidate-view c))))))
     (make-instance 'menu-item
       :menu-item-title "Unlock file(s)"
       :menu-item-action
       #'(lambda ()
           (for-selected-components
            w
            #'(lambda (c)
                (when (probe-file (path c))
                  (unlock-file (path c))
                  (setf (locked c) nil)
                  (invalidate-view c))))))

     (make-instance 'menu-item :menu-item-title "-")

     (make-instance 'menu-item
       :menu-item-title "Update Window"
       :menu-item-action
       #'(lambda ()
           (projman-update-window w)))
         )))

(defun system-dependencies-menu (system)
  (let* ((items (mapcar #'(lambda (sysname)
                            (make-instance 'menu-item
                              :menu-item-title (princ-to-string sysname)
                              :menu-item-action
                              #'(lambda ()
                                  (manage-system sysname
                                                 :auto-position
                                                 :staggerparentwindow))))
                        (make::component-depends-on system)))
         (m (make-instance 'menu
              :menu-title "Depends on"
              :menu-items items)))
    (unless items (menu-item-disable m))
    m))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun project-manager-buttons-etc (w)
  (let ((vpos (- (view-size-v w) 15)))
    (list
     (make-instance '3d-dialog-item
       :dialog-item-action
       #'(lambda (item)
           (let ((w (view-window item)))
             (when (double-click-p)
               (cond ((equal (view-size w) #@(265 14))
                      (set-view-size w (view-default-size w)))
                     (t (set-view-size w #@(265 14)))))))
       :view-nick-name :lower-pad
       :bg-color $window-background-gray
       :view-position (make-point 0 (1+ vpos))
       :view-size (make-point (- (view-size-h w) 15 ;78
                                 ) 14))
     (make-instance 'static-text-dialog-item
       :view-nick-name :status
       :dialog-item-text
       (if (system w)
         (format nil "~a files"
                 (length (make::file-components-in-component (system w))))
         "")
       :part-color-list (list :body $window-background-gray)
       :view-font '("geneva" 9 :bold)
       :view-position (make-point 9 (1+ vpos)))
     )))

;;----------------------------------------
;; Scroller
;;----------------------------------------

(defclass project-manager-scroller (ccl::scroller) ())

(defmethod scroll-bar-page-size ((view project-manager-scroller))
  (let ((vs (view-size view)))
    (make-point (point-h vs) (the fixnum (- (the fixnum (point-v vs)) 15)))))

(defmethod scroll-bar-scroll-size ((view project-manager-scroller))
  (make-point (point-h (view-size view)) 15))

(defmethod set-view-size :after ((view project-manager-scroller) h &optional v)
  (declare (ignore h v))
  (set-view-size (car (subviews view)) (view-size view)))

;;---

(defmethod initialize-instance :after ((w project-manager-window) &rest args)
  (declare (ignore args))
  (let ((sc (make-instance 'project-manager-scroller
              :h-scrollp nil
              :field-size (make-point (view-size-h w) 1000)
              :view-position #@(0 31)
              :view-size (subtract-points (view-size w) #@(15 #|30|# 46))))
        (mb (make-project-manager-menubar (system w) w)))
    (add-subviews w sc mb
                  (make-instance '3d-dialog-item
                    :dialog-item-action
                    #'(lambda (item)
                        (let ((w (view-window item)))
                          (when (double-click-p)
                            (cond ((equal (view-size w) #@(265 14))
                                   (set-view-size w (view-default-size w)))
                                  (t (set-view-size w #@(265 14)))))))
                    :view-nick-name :upper-pad
                    :bg-color $window-background-gray
                    :view-position #@(0 16)
                    :view-size (make-point (view-size-h w) 14))
                  (make-instance 'static-text-dialog-item
                    :part-color-list (list :body $window-background-gray)
                    :view-font '("geneva" 9 :bold)
                    :view-position #@(34 17)
                    :dialog-item-text "Component")
                  (make-instance 'static-text-dialog-item
                    :part-color-list (list :body $window-background-gray)
                    :view-font '("geneva" 9 :bold)
                    :view-position #@(215 17)
                    :dialog-item-text "Size"))
    (apply #'add-subviews w (project-manager-buttons-etc w))
    (setf (scroller w) sc)))

(defun manage-system (sysname &rest initargs)
  (let* ((w (apply #'make-instance
                   'project-manager-window
                   :window-type :document
                   :grow-icon-p t
                   :system (make::find-system sysname :load)
                   :window-title (string-capitalize (string sysname))
                   :window-show nil
                   initargs))
         (cv (make-component-view (make::find-system sysname :load)
                                  :view-position #@(0 0)
                                  :h-size (- (point-h (view-size w)) 15))))
    (add-subviews (scroller w) cv)
    (expand-or-collapse-component-view cv)
    (window-show w)
    ))

(defun projman-update-window (w)
  (apply #'remove-subviews (scroller w) (subviews (scroller w)))
  (let ((cv (make-component-view
             (make::find-system (intern (mk::component-name (system w))) :load)
             ; be sure to reload if the system definition has been changed
             :view-position #@(0 0)
             :h-size (- (point-h (view-size w)) 15))))
    (add-subviews (scroller w) cv)
    (expand-or-collapse-component-view cv)))

#|
(defun manage-all-systems ()
  (let* ((w (make-instance 'project-manager-window
              :window-type :document
              :grow-icon-p t
              :window-title "All Systems"
              :window-show nil))
         (v (make-instance 'list-view
              :view-position #@(0 -18)
              :view-size (make-point (- (point-h (view-size w)) 15)
                                     18))))
    (add-subviews (scroller w) v)
    (mapc #'(lambda (cv)
              (add-view-to-list v cv))
          (mapcar #'(lambda (sysname)
                      (make-component-view (make::find-system sysname :load)
                                           :view-position #@(0 0)
                                           :h-size (- (point-h (view-size w)) 15)))
                  (systems)))
    (window-show w)))
|#

(defmethod set-view-size ((w project-manager-window) h &optional v)
  (let ((hsize (view-size-h w))
        (vsize (if v v (point-v h))))
    (call-next-method w hsize vsize)
    (let ((vpos (- (view-size-v w) 15)))
      (set-view-size-h (view-named :menubar w) (view-size-h w))
      (set-view-size (scroller w) (subtract-points (view-size w) #@(15 46)))
      (set-view-size (view-named :upper-pad w) (make-point (view-size-h w) 14))
      (set-view-size (view-named :lower-pad w) (make-point (- (view-size-h w) 15) 14))
      (set-view-position (view-named :lower-pad w) (make-point 0 (1+ vpos)))
      (set-view-position (view-named :status w) (make-point 9 (1+ vpos))))))

(defmethod ccl::editing-dialogs-p ((editor project-manager-window))
  nil)
