(in-package :mcclim-desktop-launcher)

(defun edit-file (filename)
  (let ((editor (mcclim-desktop-core::find-application-1 *manager* "editor")))
    (launch-application editor filename)))

(defun register-launcher-applications (&rest names)
  (dolist (name names)
    (when (mcclim-desktop-core::find-application-1 *manager* name) 
      (push name *applications*))))
