(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (application &rest args)
	  (declare (ignore application args))
	  (desktop-app-manager:run-app-manager)))
