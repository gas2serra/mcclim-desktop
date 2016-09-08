(in-package :mcclim-desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-class-browser:browse-class 'mcclim-desktop::application)))



      
