(in-package :cl-desktop)

;;(ql:quickload "scigraph")

(setf (application-entry-fn *application*) #'(lambda (app &rest args)
					       (graph:make-demo-frame)))


