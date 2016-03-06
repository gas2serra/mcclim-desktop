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
	       :initform *trace-output*)
   (debugger-fn :initarg :debugger-fn
		:accessor manager-debugger-fn
		:initform nil)
   (config-fn :initarg :config-fn
	      :accessor manager-config-fn
	      :initform nil)))

;;; protocol: logs

(defmethod manager-log-info ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Info: ~A~%" msg)))

(defmethod manager-log-warn ((manager simple-manager-mixin) msg)
  (with-slots (log-stream) manager
    (format log-stream "Warn: ~A~%" msg)))

;;; protocol: debug

(defmethod manager-debugger-hook ((manager simple-manager-mixin) debug-p)
  (with-slots (debugger-fn) manager
    (if (or debug-p (manager-force-debug-p manager))
	debugger-fn
	nil)))

;;; protocol: configure

(defmethod configure-manager ((manager simple-manager-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (config-fn) manager
    (when config-fn
	(funcall config-fn manager))))


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

(defmethod find-application-1 ((manager standard-manager-mixin) name &optional (errorp t))
  (let ((application (get-application name nil manager)))
    (unless application
      (let ((application-file (find-file
			       (format nil *application-file-name* name))))
	(when application-file
	  (load application-file)))
      (setf application (get-application  name errorp manager)))
    application))

;;;

(defmethod configure-manager :before ((manager standard-manager-mixin) &optional force-p)
  (declare (ignore force-p))
  (let ((config-file (find-file *manager-config-file-name*)))
    (when config-file
	(if (probe-file config-file)
	    (let ((*manager* manager))
	      (load config-file))
	    (log-warn (format nil "Config file (~A) for manager not found" config-file))))))
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

;;; protocol: initialize

(defmethod initialize-instance :after ((manager standard-manager-mixin) &rest initargs)
  (declare (ignore initargs)))
