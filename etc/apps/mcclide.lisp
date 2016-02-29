(in-package :cl-desktop)

(ql:quickload "mcclide")

(defun mcclide-entry-fn (application &rest args)
  (declare (ignore args))
  (mcclide:mcclide))
     

 
(register-application "mcclide" 'standard-mcclim-application
		      :system-name "mcclide"
		      :entry-fn #'mcclide-entry-fn)


