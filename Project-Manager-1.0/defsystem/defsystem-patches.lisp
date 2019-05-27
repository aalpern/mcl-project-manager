(in-package "MAKE")

; last update 12/8/92  Alex Repenning

; set *load-verbose* to nil during OOS
(defun operate-on-system (name operation &key force
			       (version *version*)
			       (test *oos-test*) (verbose *oos-verbose*)
                               (load-source-instead-of-binary *load-source-instead-of-binary*)
                               (load-source-if-no-binary *load-source-if-no-binary*) 
			       (bother-user-if-no-binary *bother-user-if-no-binary*)
			       (compile-during-load *compile-during-load*)
			       dribble
			       (minimal-load *minimal-load*))
  (unwind-protect
      ;; Protect the undribble.
      (progn
	(when dribble (dribble dribble))
	(when test (setq verbose t))
	(when (null force);; defaults
	  (case operation
	    ((load :load) (setq force :all))
	    ((compile :compile) (setq force :new-source-and-dependents))
	    (t (setq force :all))))
	;; Some CL implementations have a variable called *compile-verbose*
	;; or *compile-file-verbose*.
	(multiple-value-bind (*version-dir* *version-replace*) 
	    (translate-version version)
	  ;; CL implementations may uniformly default this to nil
	  (let ((*load-verbose* nil) ; nil
		(*compile-file-verbose* t) ; nil
		(*compile-verbose* t) ; nil
		(*version* version)
		(*oos-verbose* verbose)
		(*oos-test* test)
		(*load-source-if-no-binary* load-source-if-no-binary)
		(*compile-during-load* compile-during-load)
		(*bother-user-if-no-binary* bother-user-if-no-binary)
		(*load-source-instead-of-binary* load-source-instead-of-binary)
		(*minimal-load* minimal-load)
		(system (find-system name :load)))
	    (unless (component-operation operation)
	      (error "Operation ~A undefined." operation))
	    (operate-on-component system operation force))))
    (when dribble (dribble))))

#+:ccl
(defun edit-operation (component force)
  "Always returns nil, i.e. component not changed."
  (declare (ignore force))
  ;;
  (let* ((full-pathname (make::component-full-pathname component :source))
         (already-editing\? #+:mcl (dolist (w (ccl:windows :class 'ccl:fred-window))
                                    (when (equal (ccl:window-filename w)
                                                 full-pathname)
                                      (return w)))
                           #-:mcl nil))
    (if already-editing\?
      #+:mcl (ccl:window-select already-editing\?) #-:mcl nil
      (ed full-pathname)))
  nil)

