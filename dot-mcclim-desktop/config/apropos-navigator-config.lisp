(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  (panter-apps:run-apropos-navigator)))



