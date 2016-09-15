(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'mcclim-panter-debugger:debugger condition me-or-my-encapsulation)))
      

