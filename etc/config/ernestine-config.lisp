(in-package :cl-desktop-user)

(setf *application* (find-application "ernestine"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (ernestine-gui:player)))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))
