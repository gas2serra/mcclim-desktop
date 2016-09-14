(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  (mcclim-panter:run-apropos-navigator)))



