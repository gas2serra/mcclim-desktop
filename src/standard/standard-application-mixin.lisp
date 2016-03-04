(in-package :cl-desktop)

;;;;
;;;; Standard Application Mixin
;;;;

(defclass standard-application-mixin ()
  ())

;;; protocols

(defgeneric application-file (application))
(defgeneric application-config-file (application))
;;(defgeneric application-loading-file (application))

;;; protocol: application file

(defmethod application-file ((application application))
  nil)

(defmethod application-file ((application standard-application-mixin))
  (with-slots (name) application
    (find-file (format nil *application-file-name* name))))

;;; protocol: application config file

(defmethod application-config-file ((application application))
  nil)

(defmethod application-config-file ((application standard-application-mixin))
  (with-slots (name) application
    (find-file (format nil *application-config-file-name* name))))

#|
(defmethod application-loading-file ((application standard-application-mixin))
  (with-slots (name) application
    (find-file (format nil *application-loading-file-name* name))))
|#

;;; protocol: application config file

(defmethod configure-application ((application standard-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (let ((config-file (application-config-file application)))
    (when config-file
      (if (probe-file config-file)
	  (let ((*application* application))
	    (load config-file))
	  (log4cl:log-warn "Config file (~A) for ~A not found" file name))))
  (call-next-method))

(defmethod load-application :before ((application standard-application-mixin) &optional force-p)
  (declare (ignore force-p))
  #|
  (let ((loading-file (application-loading-file application)))
    (when loading-file
      (if (probe-file loading-file)
	  (let ((*application* application))
	    (load loading-file))
	  (log4cl:log-warn "Loading file (~A) for ~A not found" file name)))))
  |#
  )


;;;
;;; Standard CL Application Mixin
;;;

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())

