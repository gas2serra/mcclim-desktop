(in-package :cl-desktop)
 
(register-application "emacs" 'standard-shell-application
		      :pretty-name "Emacs"
		      :make-command-fn #'(lambda (&rest args)
					   (format nil "emacs ~{~A ~}" args)))
