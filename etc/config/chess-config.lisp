(in-package :cl-desktop)

;;(ql:quickload "clim-chess")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (clim-chess:chess)))


