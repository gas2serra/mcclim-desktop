(in-package :cl-desktop)

(ql:quickload "beirc")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (beirc:beirc)))


