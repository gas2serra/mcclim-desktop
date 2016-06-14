(in-package :cl-desktop-user)

(setf *application* (find-application "closure"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (closure:start)))

(setf (application-config-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  (setf gui:*home-page* "http://www.cliki.net/")
	  nil))



