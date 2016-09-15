(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app))
	  (swank:create-server :style nil)))
