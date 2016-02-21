(in-package :cl-desktop)

(ql:quickload "climacs")

(defun climacs-rv-entry-fn (application &rest args)
  (declare (ignore args))
  (climacs:climacs-rv))
 
 
(register-application "climacs-rv" 'standard-mcclim-application
		      :system-name "climacs"
		      :entry-fn #'climacs-rv-entry-fn)


