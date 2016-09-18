(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'swank:swank-debugger-hook
		   condition me-or-my-encapsulation)))

(setf (application-debugger-fn *application*)
      #'swank:swank-debugger-hook)
      

