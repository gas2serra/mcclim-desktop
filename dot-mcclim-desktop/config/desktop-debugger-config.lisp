(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'desktop-apps:debugger
		   condition
		   me-or-my-encapsulation)))

(setf (application-debugger-fn *application*)
      #'desktop-apps:debugger)
