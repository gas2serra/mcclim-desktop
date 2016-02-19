(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (clim-pkg-doc:pkg-doc)))
      
