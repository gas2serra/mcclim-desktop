(in-package :desktop-launcher)

(defvar *applications* nil)

(defun update-applications ()
  (setf *applications*
	(sort (applications)
	      #'string<
	      :key #'application-pretty-name)))
