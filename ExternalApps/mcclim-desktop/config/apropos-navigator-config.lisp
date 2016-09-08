(in-package :mcclim-desktop-user)

;;(setf *application* (find-application "apropos-navigator"))

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (mcclim-panter:run-apropos-navigator)))



