(in-package :cl-desktop)

(defclass standard-manager-mixin ()
  ())


(defmethod find-application-1 ((manager standard-manager-mixin) name)
  (let ((application (get-application name manager)))
    (unless application
      (load-default-application-file name)
      (setf application (get-application  name manager)))
    application))

(defmethod add-application-1 :before ((manager standard-manager-mixin) (application standard-application-mixin))
  (unless (application-config-file application)
    (setf (application-config-file application)
	  (default-config-file (application-name application)))))





