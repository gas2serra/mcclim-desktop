(in-package :cl-desktop)

(ql:quickload "climon")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (climon:climon)))


