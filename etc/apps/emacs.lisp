(in-package :mcclim-desktop-user)
 
(register-application "emacs" 'standard-shell-application
		      :pretty-name "Emacs"
		      :make-command-fn #'(lambda (&rest args)
					   (format nil "emacs ~{~A ~}" args)))
