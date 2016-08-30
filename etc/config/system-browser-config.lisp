(in-package :cl-desktop-user)

(setf *application* (find-application "system-browser"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app))
	    (if (null args)
		(trivial-open-browser:open-browser "https://common-lisp.net/project/mcclim/")
		(trivial-open-browser:open-browser (car args)))))
      
(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  nil))

