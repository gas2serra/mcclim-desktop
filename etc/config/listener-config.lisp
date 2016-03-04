(in-package :cl-desktop)

;;(asdf:require-system "clim-listener")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (clim-listener:run-listener)))


