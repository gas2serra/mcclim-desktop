(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (climc:start-climc)))




