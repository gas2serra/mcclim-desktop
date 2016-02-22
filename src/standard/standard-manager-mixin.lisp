(in-package :cl-desktop)

;;;;
;;;; Standard Manager Mixin
;;;;

(defclass standard-manager-mixin ()
  ())

(defmethod find-application-1 ((manager standard-manager-mixin) name)
  (let ((application (get-application name manager)))
    (unless application
      (let ((application-file (find-application-file name)))
	(when application-file
	  (load application-file)))
      (setf application (get-application  name manager)))
    application))

(defun setup ()
  (uiop:ensure-all-directories-exist (list *user-directory*)))

