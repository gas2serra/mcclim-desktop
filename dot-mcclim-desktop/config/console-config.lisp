(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (application &rest args)
	  (declare (ignore application args))
	  (desktop-console:run-console)))
