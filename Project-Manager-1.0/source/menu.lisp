;; -*- mode:lisp; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T   M A N A G E R
;;
;; file: 	menu.lisp
;; author: 	Adam Alpern
;; created: 	7/22/1995
;;
;;	Setup the projects menu.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Revision History
;; ----------------
;; 12/03/96	- added "Load" and "Compile" sub-menus
;; 7/19/96	- #'systems use :from-end t in call to subseq. Now systems
;;		  with a period in the name will parse properly
;; 		- added icon to about box
;; 04/05/95	- added "Show Systems Dialog" item
;; 04/02/96	- added menu-utils, by David B. Lamkins
;; 11/30/95	- added "Color-P" menu item
;;		- added about box
;; 11/29/95     - (David B. Lamkins) Introduced *PROJMAN-COLOR-P* variable.
;;		  (moved to .system file - ala)
;; 11/10/95	- updated my email address.
;; 07/22/95	- file created
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; menu-utils, by dbl
;; Add a new generic function to make menu manipulation a little easier.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric insert-menu-items (menu test-after test-remove &rest menu-items)
  (:documentation
   "Insert menu-items into menu after first menu item to satisfy test-after
predicate.  Any existing menu items satisfying test-remove predicate are
removed from the menu.  If test-after is nil, menu-items are inserted at
the start of the menu.  If test-remove is nil, existing items having the
same title as any of the inserted menu-items are removed from the menu."))

(defmethod insert-menu-items ((menu menu) test-after (test-remove (eql nil)) &rest menu-items)
  (apply #'insert-menu-items
         menu
         test-after
         #'(lambda (existing-mi)
             (let ((existing-mi-title
                    (menu-item-title existing-mi)))
               (some #'(lambda (mi)
                         (string-equal
                          (menu-item-title mi)
                          existing-mi-title))
                     menu-items)))
         menu-items))

(defmethod insert-menu-items ((menu menu) (test-after (eql nil)) test-remove &rest menu-items)
  (let ((all-menu-items (menu-items menu)))
    (apply #'remove-menu-items menu all-menu-items)
    (apply #'add-menu-items menu menu-items)
    (dolist (menu-item all-menu-items)
      (unless (funcall test-remove menu-item)
        (add-menu-items menu menu-item)))))

(defmethod insert-menu-items ((menu menu) test-after test-remove &rest menu-items)
  (let ((all-menu-items (menu-items menu))
        (inserted nil))
    (apply #'remove-menu-items menu all-menu-items)
    (dolist (menu-item all-menu-items)
      (unless (funcall test-remove menu-item)
        (add-menu-items menu menu-item))
      (unless inserted
        (when (funcall test-after menu-item)
          (apply #'add-menu-items menu menu-items)
          (setq inserted t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project manager menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *project-menu* nil)

(defun systems ()
  "Returns a list of the names of all the systems residing in the
directory make::*central-registry*"
  (let ((systems nil))
    (if (listp make:*central-registry*)
      (dolist (dir make:*central-registry*)
        (dolist (file (directory
                       (concatenate 'string
                                    (princ-to-string dir)
                                    "*.system")
                       :test #'(lambda (f)
                                 (equal (ccl::mac-file-type f)
                                        :text))))
          (push (ccl::keywordify (read-from-string
                                  (subseq (file-namestring file) 0
                                          (position #\.
                                                    (file-namestring file)))))
                systems)))
      (dolist (file (directory
                     (concatenate 'string
                                  (princ-to-string make:*central-registry*)
                                  "*.system")
                     :test #'(lambda (f)
                               (equal (ccl::mac-file-type f)
                                      :text))))
        (push (ccl::keywordify (read-from-string
                                (remove #\∂ (subseq (file-namestring file) 0
                                                    (position #\.
                                                              (file-namestring file)
                                                              :from-end t)))))
              systems)))
    systems))

(defun about-project-manager ()
  (modal-dialog
   (make-instance
     'color-dialog
     :window-type  :double-edge-box
     :view-size
     #@(300 150)
     :close-box-p
     nil
     :view-font
     '("Chicago" 12 :srcor :plain (:color-index 0))
     :view-subviews
     (list (make-dialog-item
            'button-dialog-item
            #@(111 121)
            #@(66 20)
            "OK"
            #'(lambda (item)
                (declare (ignore item))
                (return-from-modal-dialog nil))
            :default-button
            t)
           (make-dialog-item
            'static-text-dialog-item
            #@(70 39)
            #@(116 16)
            "alpern@brightware.com"
            'nil
            :view-font
            '("Geneva" 9 :srcor :plain (:color-index 0)))
           (make-dialog-item
            'static-text-dialog-item
            #@(16 39)
            #@(52 16)
            "Internet:"
            'nil
            :view-font
            '("Geneva" 9 :srcor :bold (:color-index 0)))
           (make-dialog-item
            'static-text-dialog-item
            #@(17 21)
            #@(250 14)
            "Copyright © 1995-2004, Adam Alpern.
All Rights Reserved."
            'nil
            :view-font
            '("Geneva" 9 :srcor :plain (:color-index 0)))
           (make-dialog-item
            'static-text-dialog-item
            #@(17 58)
            #@(262 28)
            "A visual project manager for use with Mark Kantrowitz's portable Defsystem utility."
            'nil
            :view-font
            '("Geneva" 9 :srcor :plain (:color-index 0)))
           (make-instance 'cicn-dialog-item
             :cicn *projman-icon*
             :cicn-size #@(18 14)
             :view-size #@(18 14)
             :view-position #@(17 6))
           (make-dialog-item
            'static-text-dialog-item
            #@(39 7)
            #@(154 16)
            "Defsystem Project Manager"
            'nil
            :view-font
            '("Geneva" 9 :srcor :bold (:color-index 0)))
           (make-dialog-item
            'static-text-dialog-item
            #@(193 7)
            #@(44 16)
            *project-manager-version*
            'nil
            :view-nick-name
            :version
            :view-font
            '("Geneva" 9 :srcor :bold (:color-index 0)))))))

(defun project-menu-oos-items (operation)
  (mapcar #'(lambda (sysname)
              (make-instance 'menu-item
                :menu-item-title
                (string-capitalize (string sysname))
                :menu-item-action #'(lambda ()
                                      (eval-enqueue `(oos ,sysname ,operation)))))
           (systems)))

(defun project-menu-items ()
  (nconc
   (list (make-instance 'menu-item
           :menu-item-title "About Project Manager…"
           :menu-item-action 'about-project-manager)
         (make-instance 'menu-item
           :menu-item-title "-"))
   (mapcar #'(lambda (sysname)
               (make-instance 'menu-item
                 :menu-item-title
                 (string-capitalize (string sysname))
                 :menu-item-action #'(lambda ()
                                       (manage-system sysname))))
           (systems))
   (list (make-instance 'menu-item
           :menu-item-title "-")
         (make-instance 'menu
           :menu-items (project-menu-oos-items :load)
	   :menu-title "Load")
         (make-instance 'menu
           :menu-items (project-menu-oos-items :compile)
	   :menu-title "Compile")
         (make-instance 'menu-item
           :menu-item-title "-")
         #|
	(make-instance 'menu-item
           :menu-item-title "Color-P"
           :menu-item-checked *project-manager-color-p*
           :menu-item-action
           #'(lambda ()
               (if (and (menu-item-check-mark
                         (find-menu-item *project-menu* "Color-P"))
                        *project-manager-color-p*)
                 (progn
                   (set-menu-item-check-mark
                    (find-menu-item *project-menu* "Color-P") nil)
                   (setq *project-manager-color-p* nil))
                 (progn
                   (set-menu-item-check-mark
                    (find-menu-item *project-menu* "Color-P") t)
                   (setq *project-manager-color-p* t))
                 )))
	|#

         (make-instance 'menu-item
           :menu-item-title "Preferences…"
           :menu-item-action
           #'(lambda ()
               (modal-dialog
                (make-instance 'preference-dialog
                  :sheets  (projman-preference-sheets)))))
         (make-instance 'menu-item
           :menu-item-title "-")
         (make-instance 'menu-item
           :menu-item-title "Show Systems Dialog"
           :menu-item-action 'systems-dialog)
         (make-instance 'menu-item
           :menu-item-title "Update Menu"
           :menu-item-action 'update-project-menu)
         )
   ))



(defun project-menu ()
  (make-instance 'menu
    :menu-title "Project"
    :menu-items (project-menu-items)
    ))


(defun setup-project-menu ()
  (if (find-menu "Project") (menu-deinstall (find-menu "Project")))
  (if *project-manager-use-menu*
    (progn (setf *project-menu* (project-menu))
           (menu-install *project-menu*))
    (insert-menu-items *tools-menu*
                       #'(lambda (mi)
                           (string-equal "Processes" (menu-item-title mi)))
                       nil
                       (make-instance 'menu-item
                         :menu-item-title "Systems"
                         :menu-item-action #'(lambda ()
                                               (systems-dialog))
                         :help-spec "Displays a list of systems.")
                       )))

(defun update-project-menu ()
  (cond ((and *project-menu*
              (menu-installed-p *project-menu*))
         ; if it's installed
         (apply #'remove-menu-items *project-menu* (menu-items *project-menu*))
         (apply #'add-menu-items *project-menu* (project-menu-items)))
        (t	; else install it
         (setup-project-menu))
    ))

;(eval-when (:load-toplevel :execute)
;  (setup-project-menu))
