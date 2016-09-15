(in-package :desktop-user)
 
(register-application "slime" 'standard-shell-application
		      :pretty-name "Slime"
		      :make-command-fn #'(lambda (&rest args)
					   (format nil "emacs --eval '(slime-connect \"127.0.0.1\" 4005)'" args)))
