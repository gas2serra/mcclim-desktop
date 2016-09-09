(in-package :mcclim-desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app))
	    (if (null args)
		(trivial-open-browser:open-browser "https://common-lisp.net/project/mcclim/")
		(trivial-open-browser:open-browser (car args)))))
      

