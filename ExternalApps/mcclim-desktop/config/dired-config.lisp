(in-package :mcclim-desktop-user)

(setf *application* (find-application "dired"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (ftd:ftd "/home/")))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))


