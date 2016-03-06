(in-package :cl-desktop-user)

(setf *application* (find-application "beirc"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (beirc:beirc :new-process nil)))

(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))
