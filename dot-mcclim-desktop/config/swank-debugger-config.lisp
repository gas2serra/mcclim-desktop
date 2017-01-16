(in-package :desktop-user)

(defun desktop-swank-debugger-hook (condition me-or-my-encapsulation)
  (unless swank::*connections*
    (launch-app :swank-server)
    (sleep 3.0)
    (launch-app :slime)
    (sleep 5.0))
  (funcall #'swank:swank-debugger-hook condition me-or-my-encapsulation))

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'desktop-swank-debugger-hook
		   condition me-or-my-encapsulation)))

(setf (application-debugger-fn *application*)
      (lambda (condition me-or-my-encapsulation)
	(funcall #'desktop-swank-debugger-hook
		 condition me-or-my-encapsulation)))
      

