(in-package :cl-desktop-user)

(setf *application* (find-application "clim-demo"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-demo::demodemo)))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))
