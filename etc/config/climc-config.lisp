(in-package :cl-desktop)

(ql:quickload "climc")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (climc:start-climc)))


