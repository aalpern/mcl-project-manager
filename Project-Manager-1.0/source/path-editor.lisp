;; -*- mode:lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; system:	P R O J E C T - M A N A G E R
;;
;; file:	path-editor.lisp
;; author: 	Adam Alpern
;; date:	6/26/95
;;
;; rewrite of Richard Lynch's pathedit.lisp, which is
;; � MCMXCV Learning Sciences Corporation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass path-editor-dialog (color-dialog)
  (add-button delete-button ok-button cancel-button pathlist
              (pathnames :initarg :pathnames :initform nil :accessor pathnames))
  (:default-initargs :view-size #@(400 200)
    :content-color 	$window-background-gray))

(defclass pathname-sequence-dialog-item (3d-sequence-dialog-item)
  ((pathnames :initarg :pathnames :initform nil :accessor pathnames :type 'list))
  (:default-initargs
    :table-sequence nil
    :table-vscrollp t
    :table-hscrollp nil
    :selection-type :disjoint
    :table-print-function #'(lambda (object stream)
                              (princ (mac-namestring object) stream))))

(defmethod (setf pathnames) :after (new-paths (view pathname-sequence-dialog-item))
  (set-table-sequence view new-paths))

(defmethod initialize-instance :after ((w path-editor-dialog) &rest args)
  (declare (ignore args))
  (let ((ab (make-instance '3d-button-dialog-item
              :square t :inset t :frame t
              :dialog-item-text "Add..."
              :view-position (make-point 2 (- (view-size-v w) 24))
              :view-size #@(60 20)
              :View-font '("geneva" 9 :plain)
              :dialog-item-action
              #'(lambda (item)
                  (let ((newbie (choose-directory-dialog)))
                    (pushnew newbie
                             (pathnames (slot-value (view-window item) 'pathlist))
                             :test #'equalp)))
              ))
        (db (make-instance '3d-button-dialog-item
              :square t :inset t :frame t
              :view-size #@(60 20)
              :view-position (make-point 66 (- (view-size-v w) 24))
              :View-font '("geneva" 9 :plain)
              :dialog-item-text "Delete"
              :dialog-item-action
              #'(lambda (item)
                  (let* ((table (slot-value (view-window item) 'pathlist))
                         (cells (selected-cells table))
                         (goners (mapcar #'(lambda (c) (cell-contents table c)) cells)))
                    (dolist (cell cells) (cell-deselect table cell))
                    (setf (pathnames table)
                          (nset-difference (pathnames table) goners :test #'equalp))
                    (dialog-item-disable item)))))
        (cb (make-instance '3d-button-dialog-item
              :square t :inset t :frame t
              :view-size #@(60 20)
              :view-position (make-point (- (view-size-h w) 134)
                                         (- (view-size-v w) 24))
              :View-font '("geneva" 9 :plain)
              :dialog-item-text "Cancel"
              :dialog-item-action
              #'(lambda (i)
                  (declare (ignore i))
                  (return-from-modal-dialog :cancel))
              ))
        (ob (make-instance '3d-button-dialog-item
              :square t :inset nil :frame t
              :view-size #@(60 20)
              :view-position (make-point (- (view-size-h w) 66)
                                         (- (view-size-v w) 24))
              :View-font '("geneva" 9 :plain)
              :dialog-item-text "OK"
              :default-button t
              :dialog-item-action
              #'(lambda (item)
                  (declare (ignore item))
                  (return-from-modal-dialog t))))
        (pl (make-instance 'pathname-sequence-dialog-item
              :View-font '("monaco" 9 :plain)
              :view-position #@(4 4)
              :view-size (subtract-points (view-size w) #@(6 34))
              :dialog-item-action
              #'(lambda (item)
                  (if (selected-cells item)
                    (dialog-item-enable (slot-value (view-window item) 'delete-button))
                    (dialog-item-disable (slot-value (view-window item) 'delete-button))
                    )))))
    (setf (slot-value w 'add-button) ab)
    (setf (slot-value w 'delete-button) db)
    (setf (slot-value w 'ok-button) ob)
    (setf (slot-value w 'cancel-button) cb)
    (setf (slot-value w 'pathlist) pl)
    (add-subviews w ab db ob cb pl)
    (set-table-sequence pl (pathnames w))
    ))

(defun edit-pathnames (pathname-list)
  (let ((old (copy-list pathname-list))
        result dialog)
    (setf dialog (make-instance 'path-editor-dialog
                   :pathnames pathname-list
                   :window-type :double-edge-box))
    (setf result (catch-cancel (modal-dialog dialog)))
    (if (eq result :cancel)
      old
      (pathnames (slot-value dialog 'pathlist)))))
