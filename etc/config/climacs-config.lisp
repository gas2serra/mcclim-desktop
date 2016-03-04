(in-package :cl-desktop)

(asdf:require-system "climacs")

(defun climacs-entry-fn (application &rest args)
  (declare (ignore args))
  (if (null args)
      (climacs:climacs)
      (climacs:edit-file (car args))))

(setf (application-entry-fn *application*) #'climacs-entry-fn)


