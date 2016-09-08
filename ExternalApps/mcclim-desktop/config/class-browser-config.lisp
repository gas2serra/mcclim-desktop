(in-package :mcclim-desktop-user)

;(setf *application* (find-application "class-browser"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-class-browser:browse-class 'cl-desktop::application)))
				    
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))


      
