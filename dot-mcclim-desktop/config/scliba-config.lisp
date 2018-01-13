(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app object &rest args)
	  (declare (ignore app))
	  (scliba-gui:scliba-gui)))


