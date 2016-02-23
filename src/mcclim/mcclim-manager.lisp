(in-package :mcclim-desktop)

(defclass mcclim-manager (standard-manager-mixin
			  simple-manager-mixin
			  manager)
  ())

(defmethod initialize-instance :after ((manager mcclim-manager) &rest initargs)
  (declare (ignore initargs))
  ;;(setf *debugger-hook* nil)
  ;;(setf *debugger-hook* #'clim-debugger:debugger)
  )

(defmethod manager-debugger-hook ((manager mcclim-manager) debug-p)
  (if debug-p 
      #'clim-debugger:debugger
      nil))

