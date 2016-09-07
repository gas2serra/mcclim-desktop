(in-package :cl-desktop-user)

(setf *application* (find-application "task-manager"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (mcclim-panter:run-task-manager)))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))


