(in-package :mcclim-desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app))
	    (if (null args)
		(climacs:climacs)
		(climacs:edit-file (car args)))))
      
(in-package drei)

(define-command (com-paste-x-clipboard :name t :command-table drei:editing-table) ()
  (flexichain:insert-sequence (point) (uiop:run-program "xclip -selection clipboard -o" :output :string)))

(define-command (com-copy-x-clipboard :name t :command-table drei:view-table) ()
  (with-input-from-string (input-stream (coerce (kill-ring-yank *kill-ring*) 'string))
    (uiop:run-program "xclip -selection clipboard -i " :output nil :input input-stream)))

(esa-io::set-key 'com-paste-x-clipboard 'drei:editing-table '((#\y :META :CONTROL)))
(esa-io::set-key 'com-copy-x-clipboard 'drei:view-table '((#\w :META :CONTROL)))
