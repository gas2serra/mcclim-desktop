(in-package :cl-desktop)

;;;;
;;;; Application Mixin
;;;; 

;;;
;;; Simple Application Mixin
;;;

(defclass simple-application-mixin ()
  ((entry-fn :initarg :entry-fn
	     :accessor application-entry-fn
	     :initform nil)
   (config-fn :initarg :config-fn
	      :accessor application-config-fn
	      :initform nil)))

(defmethod run-application ((application simple-application-mixin) &rest args)
  (with-slots (entry-fn name) application
    (if entry-fn
	(apply entry-fn application args)
	(log-warn (format nil "Entry function for ~A undefined" name)))))

(defmethod configure-application ((application simple-application-mixin) &optional force-p)
  (with-slots (config-fn config-file name) application
    (if config-fn
	(funcall config-fn application)
	(log-warn (format nil "Config function for ~A undefined" name)))))

;;;
;;; Simple CL Appplication Mixin
;;;

(defclass simple-cl-application-mixin (simple-application-mixin)
  ((loading-fn :initarg :loading-fn
	       :accessor application-loading-fn
	       :initform nil)
   (installing-fn :initarg :installing-fn
	       :accessor application-installing-fn
	       :initform nil)))
   
(defmethod load-application ((application simple-cl-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (loading-fn name) application
    (if loading-fn
	(apply loading-fn application)
	(log-warn (format nil "Loading function for ~A undefined" name)))))

(defmethod install-application ((application simple-cl-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (install-fn name) application
    (if install-fn
	(apply install-fn application)
	(log-warn (format nil "Installing function for ~A undefined" name)))))

;;;
;;; Simple Shell Mixin
;;;

(defclass simple-shell-application-mixin ()
  ((make-command-fn :initarg :make-command-fn
		    :accessor shell-application-make-command-fn
		    :initform nil)))

(defmethod run-application ((application simple-shell-application-mixin) &rest args)
  (with-slots (make-command-fn name) application
    (if make-command-fn
	(uiop:run-program (apply make-command-fn args))
	(log-warn (format nil "Make command function for ~A undefined" name)))))

;;;;
;;;; ASDF mixin
;;;;

(defclass simple-asdf-cl-application-mixin ()
  ())

(defmethod load-application :before ((application simple-asdf-cl-application-mixin) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-name debug-system-p name) application
    (if system-name
	(if debug-system-p
	    (asdf:operate 'asdf:load-source-op system-name :force-not t)
	    (asdf:operate 'asdf:load-op system-name))
	(log-warn (format nil "System name for ~A undefined" name)))))

(defmethod install-application :before ((application simple-asdf-cl-application-mixin) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-name) application
    (if system-name
	(ql:quickload system-name)
	(log-warn (format nil "System name for ~A undefined" name)))))

;;;;
;;;; Standard Application Mixin
;;;;

(defclass standard-application-mixin ()
  ())

;;; protocols

(defgeneric application-file (application))
(defgeneric application-config-file (application))

;;; protocol: application file

(defmethod application-file ((application standard-application-mixin))
  (with-slots (name) application
    (find-file (format nil *application-file-name* name))))

;;; protocol: application config file

(defmethod application-config-file ((application standard-application-mixin))
  (with-slots (name) application
    (find-file (format nil *application-config-file-name* name))))

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

;;;
;;; Standard CL Application Mixin
;;;

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())

