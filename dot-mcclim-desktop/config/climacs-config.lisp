(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app))
	    (if (null args)
		(climacs:climacs)
		(climacs::%edit-file (car args)))))

;;;
;;; patch
;;;
(in-package :climacs)

(defun %edit-file (thing &rest args
                  &key (process-name "Climacs") (width 900) (height 400)
		  (text-style *climacs-text-style*))
  "Edit THING in an existing climacs process or start a new one. THING
can be a filename (edit the file) or symbol (edit its function definition)."
  (declare (ignore process-name width height text-style))
  (let ((climacs-frame (find-climacs-frame))
        (command 
         (typecase thing
           (null nil)
           (symbol (list 'drei-lisp-syntax::com-edit-definition thing))
           ((or string pathname)
            (truename thing)  ; raise file-error if file doesn't exist
            (list 'esa-io::com-find-file thing))
           (t (error 'type-error :datum thing
                     :expected-type '(or null string pathname symbol))))))
    (if climacs-frame
        (when command
          (execute-frame-command climacs-frame command))
        (apply #'climacs-common command args)))
  t)


;;;
;;; clipboard
;;;

(in-package drei)

(define-command (com-paste-x-clipboard :name t :command-table drei:editing-table) ()
  (flexichain:insert-sequence (point) (desktop-sys:paste-from-x11-clipboard)))

(define-command (com-copy-x-clipboard :name t :command-table drei:view-table) ()
  (desktop-sys:copy-to-x11-clipboard (coerce (kill-ring-yank *kill-ring*) 'string)))

(esa-io::set-key 'com-paste-x-clipboard 'drei:editing-table '((#\y :META :CONTROL)))
(esa-io::set-key 'com-copy-x-clipboard 'drei:view-table '((#\w :META :CONTROL)))

;;;
;;; Apropos navigator
;;;

(in-package :desktop-user)

(clim:define-command (com-apropos-navigator
		      :name t 
		      :command-table drei:editing-table) ()
  (let ((return-point (drei:point))
	(return-values (run-app "apropos-navigator")))
    (when return-values 
      (flexichain:insert-sequence return-point
				  (write-to-string return-values)))))

(esa-io::set-key 'com-apropos-navigator
		 'drei-lisp-syntax::lisp-table '((#\s :meta :control)))

