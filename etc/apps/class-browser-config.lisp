(in-package :cl-desktop)

(ql:quickload "clim-class-browser")

(setf (application-entry-fn *application*)
      #'(lambda (&rest args)
	  (clim-class-browser:browse-class 'application)))
				    

      
