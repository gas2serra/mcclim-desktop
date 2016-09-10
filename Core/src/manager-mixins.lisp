(in-package :mcclim-desktop-core)

;;;;
;;;; Manager Mixin
;;;; 

;;;
;;; Simple Manager Mixin
;;;
#|
(defclass simple-manager-mixin ()
  ((log-stream :initarg :log-stream
	       :accessor manager-log-stream
	       :initform *trace-output*)))
|#
;;; protocol: logs
#|
(defmethod manager-log-info ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Info: ~A~%" msg)))

(defmethod manager-log-warn ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Warn: ~A~%" msg)))
|#
;;;;
;;;; Standard Manager Mixin
;;;;

(defclass standard-manager-mixin ()
  ())

;;;
;;; protocols 
;;;

(defgeneric load-application-file (manager name &optional force-p))
(defgeneric reload-application-files (manager))

;;; protocol: discover

(defmethod discover-application ((manager standard-manager-mixin) name &optional (errorp t))
  (load-application-file manager name)
  (get-application manager name errorp))

(defmethod discover-applications ((manager standard-manager-mixin))
  (dolist (name (find-all-application-names))
    (load-application-file manager name t)))


;;; protocol: configure

(defmethod configure-manager :before ((manager standard-manager-mixin) &optional force-p)
  (declare (ignore force-p))
  (let ((config-file (find-file *manager-config-file-name*)))
    (if config-file
	(let ((*manager* manager))
	  (load config-file))
	(log-warn (format nil "Config file (~A) for manager not found" *manager-config-file-name*)))))

;;; protocol: load

(defmethod load-application-file (manager name &optional force-p)
  (let ((application (get-application manager name nil)))
    (when (or (null application) force-p)
      (let ((application-file
	     (find-application-file name)))
	(when application-file
	  (load application-file))))))

(defmethod reload-application-files ((manager standard-manager-mixin))
  (with-slots (name->application) manager
    (maphash #'(lambda (name application)
		 (declare (ignore application))
		 (load-application-file manager name t))
	     name->application)))

;; initialize

(defmethod initialize-instance :after ((manager manager) &rest initargs)
  (declare (ignore manager initargs))
  (uiop:ensure-all-directories-exist (list *user-directory*
					   (uiop:merge-pathnames*
					    *application-file-directory*
					    *user-directory*)
					   (uiop:merge-pathnames*
					    *application-config-directory*
					    *user-directory*))))
  
