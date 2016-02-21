(in-package :cl-desktop)

 
(register-application "emacs" 'standard-shell-application
		      :make-command-fn #'(lambda (&rest args)
					   (format nil "emacs ~{~A ~}" args)))
