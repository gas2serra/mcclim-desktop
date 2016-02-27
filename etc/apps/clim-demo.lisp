(in-package :cl-desktop)

(ql:quickload "clim-examples")

(defun clim-demo-entry-fn (application &rest args)
  (declare (ignore args))
  (clim-demo::demodemo))
  
(register-application "clim-demo" 'standard-mcclim-application
		      :system-name "clim-examples"
		      :entry-fn #'clim-demo-entry-fn)


