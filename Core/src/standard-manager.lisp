(in-package :cl-desktop)

(defclass standard-manager (standard-manager-mixin
			    simple-manager-mixin
			    manager)
  ())

(defun make-standard-manager ()
  (setf *manager* (make-instance 'standard-manager)))
