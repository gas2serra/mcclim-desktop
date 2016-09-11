(in-package :mcclim-desktop-core)

;;;;
;;;; Application discovery
;;;;

(defun discover-application (name &optional force-p)
  (let ((application (find-registered-application name nil)))
    (when (or force-p (null application))
      (let ((application-file
	     (find-application-file name)))
	(when application-file
	  (load application-file)))))
  (find-registered-application name))
  
(defun discover-applications (&optional force-p)
  (dolist (name (find-all-application-names))
    (discover-application name force-p)))

(defun refresh-application (name)
  (discover-application name t))

(defun refresh-applications ()
  (remove-registered-applications)
  (discover-applications t))



