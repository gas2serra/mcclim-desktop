(in-package :cl-desktop)

;;(ql:quickload "clim-pkg-doc")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (clim-pkg-doc:pkg-doc)))


