(in-package :mcclim-desktop-user)

(defun mcclide-entry-fn (application &rest args)
  (declare (ignore application args))
  (mcclide:mcclide))

(setf (application-entry-fn *application*) #'mcclide-entry-fn)


