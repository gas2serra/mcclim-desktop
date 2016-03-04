(in-package :cl-desktop)

;;(asdf:require-system "mcclide")

(defun mcclide-entry-fn (application &rest args)
  (declare (ignore args))
  (mcclide:mcclide))

(setf (application-entry-fn *application*) #'mcclide-entry-fn)


