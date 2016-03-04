(in-package :cl-desktop)

;;(asdf:require-system "clim-examples")

(defun clim-demo-entry-fn (application &rest args)
  (declare (ignore args))
  (clim-demo::demodemo))
  
(setf (application-entry-fn *application*) #'clim-demo-entry-fn)


