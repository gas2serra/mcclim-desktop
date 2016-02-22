(in-package :mcclim-desktop)

(defclass mcclim-manager (standard-manager-mixin
			  simple-manager-mixin
			  manager)
  ())

(defmethod inizialize-instance :after ((manager mcclim-manager) &rest initargs)
  (setf *debugger-fn* #'clim-debugger:debugger))

(defun init ()
  (setf *manager* (make-instance 'mcclim-desktop::mcclim-manager)))
