(in-package :cl-desktop)

;;;;
;;;; Manager Mixin
;;;; 

;;;
;;; Simple Manager Mixin
;;;

(defclass simple-manager-mixin ()
  ((log-stream :initarg :log-stream
	       :accessor manager-log-stream
	       :initform *trace-output*)))

;;; protocol: logs

(defmethod manager-log-info ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Info: ~A~%" msg)))

(defmethod manager-log-warn ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Warn: ~A~%" msg)))

;;;;
;;;; Standard Manager Mixin
;;;;

(defclass standard-manager-mixin ()
  ())

;;;
;;; protocols 
;;;

(defgeneric manager-setup (manager))
(defgeneric reload-application-files (manager))

;;; protocol: find

(defmethod find-application-1 ((manager standard-manager-mixin) name)
  (let ((application (get-application name manager)))
    (unless application
      (let ((application-file (find-file
			       (format nil *application-file-name* name))))
	(when application-file
	  (load application-file)))
      (setf application (get-application  name manager)))
    application))



;;;

(defmethod manager-setup ((manager standard-manager-mixin))
  (uiop:ensure-all-directories-exist (list *user-directory*)))

(defmethod reload-application-files ((manager standard-manager-mixin))
  (with-slots (name->application) manager
    (maphash #'(lambda (name application)
		 (declare (ignore application))
		 (let ((application-file (find-file
					  (format nil *application-file-name* name))))
		   (when application-file
		     (load application-file))))
		 name->application)))

;;; protocol" initialize

(defmethod initialize-instance :after ((manager standard-manager-mixin) &rest initargs)
  (declare (ignore initargs))
  (let ((init-file (find-file *init-file-name*)))
    (when init-file
      (load init-file))))
