(in-package :mcclim-desktop-launcher)

(defvar *applications* nil)

(defun update-applications ()
  (setf *applications*
	(sort (remove-if-not #'application-menu-p (applications))
	      #'string<
	      :key #'application-pretty-name)))
