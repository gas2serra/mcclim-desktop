(in-package :mcclim-desktop-launcher)

(defun edit-file (filename)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename))))

#|
(defun register-launcher-applications (&rest names)
  (dolist (name names)
    (when (find-application name) 
      (push name *applications*))))
|#
