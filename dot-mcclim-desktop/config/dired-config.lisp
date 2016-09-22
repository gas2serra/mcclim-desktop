(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (ftd:ftd *DEFAULT-PATHNAME-DEFAULTS*)))


