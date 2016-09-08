(in-package :mcclim-desktop-user)

(setf *application* (find-application "pkg-doc"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-pkg-doc:pkg-doc)))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))

