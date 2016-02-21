(in-package :cl-desktop)

(ql:quickload "clim-class-browser")

(register-application "class-browser" 'standard-mcclim-application
		      :system-name "clim-class-browser"
		      :entry-fn #'(lambda (&rest args)
				    (clim-class-browser:browse-class 'application)))
				    

      
