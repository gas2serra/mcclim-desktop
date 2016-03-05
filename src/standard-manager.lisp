(in-package :cl-desktop)

(defclass standard-manager (standard-manager-mixin
			    simple-manager-mixin
			    manager)
  ())
