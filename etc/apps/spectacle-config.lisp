(in-package :cl-desktop)

(ql:quickload "spectacle")

(setf (application-entry-fn *application*) #'(lambda (app &rest args)
					       (spectacle:spectacle)))



