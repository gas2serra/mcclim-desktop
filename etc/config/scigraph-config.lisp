(in-package :cl-desktop)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (graph:make-demo-frame)))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))
