(in-package :cl-desktop-user)

(setf *application* (find-application "mcclide"))

(defun mcclide-entry-fn (application &rest args)
  (declare (ignore application args))
  (mcclide:mcclide))

(setf (application-entry-fn *application*) #'mcclide-entry-fn)


