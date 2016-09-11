(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-class-browser:browse-class 'desktop-user::application)))



      
