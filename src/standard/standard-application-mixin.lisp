(in-package :cl-desktop)

(defclass standard-application-mixin ()
  ())

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())


(defmethod application-file ((application standard-application-mixin))
  (with-slots (name) application
    (default-application-file name)))

