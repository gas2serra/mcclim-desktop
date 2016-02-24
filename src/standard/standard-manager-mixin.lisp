(in-package :cl-desktop)

;;;;
;;;; Standard Manager Mixin
;;;;

(defclass standard-manager-mixin ()
  ())

(defmethod find-application-1 ((manager standard-manager-mixin) name)
  (let ((application (get-application name manager)))
    (unless application
      (let ((application-file (find-file
			       (format nil *application-file-name* name))))
	(when application-file
	  (load application-file)))
      (setf application (get-application  name manager)))
    application))

(defmethod manager-setup ((manager standard-manager-mixin))
  (uiop:ensure-all-directories-exist (list *user-directory*)))

(defmethod initialize-instance :after ((manager standard-manager-mixin) &rest initargs)
  (declare (ignore initargs))
  (let ((init-file (find-file *init-file-name*)))
    (when init-file
      (load init-file))))
