(in-package :mcclim-desktop-core)

;;;;
;;;; Standard Application
;;;;

(defclass standard-cl-application (standard-cl-application-mixin
				   simple-cl-application-mixin
				   cl-application)
  ())

(defclass standard-mcclim-application (standard-cl-application-mixin
				       simple-cl-application-mixin
				       mcclim-application)
  ())
   
(defclass standard-alias-application (standard-application-mixin
				      alias-application)
  ())

(defclass standard-link-application (standard-application-mixin
				     link-application)
  ())

(defclass standard-shell-application (standard-application-mixin
				      simple-shell-application-mixin
				      shell-application)
  ())
