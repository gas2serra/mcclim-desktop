(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app)
	  (declare (ignore app))
	  (desktop-apps:run-apropos-navigator)))



