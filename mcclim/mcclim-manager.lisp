(in-package :mcclim-desktop)

(defclass mcclim-manager (standard-manager-mixin
			  simple-manager-mixin
			  manager)
  ())

(defmethod initialize-instance :after ((manager mcclim-manager) &rest initargs)
  (declare (ignore initargs))
  (setf (manager-debugger-fn manager) #'clim-debugger:debugger))

