(in-package :cl-desktop-user)

(setf *application* (find-application "chess"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-chess:chess)))

(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))



