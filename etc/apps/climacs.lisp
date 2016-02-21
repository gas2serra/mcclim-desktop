(in-package :cl-desktop)

(ql:quickload "climacs")

(defun climacs-entry-fn (application &rest args)
  (declare (ignore args))
  (if (null args)
      (climacs:climacs)
      (climacs:edit-file (car args))))

 
(register-application "climacs" 'standard-mcclim-application
		      :system-name "climacs"
		      :entry-fn #'climacs-entry-fn)


