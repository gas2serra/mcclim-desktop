(in-package :cl-desktop)

;;;
;;; Simple Application
;;;

(defclass simple-application-mixin ()
  ((entry-fn :initarg :entry-fn
	     :accessor application-entry-fn
	     :initform nil)
   (config-fn :initarg :config-fn
	      :accessor application-config-fn
	      :initform nil)
   (config-file :initarg :config-file
		:initform nil
		:accessor application-config-file)))

(defmethod run-application ((application simple-application-mixin) &rest args)
  (with-slots (entry-fn name) application
    (if entry-fn
	(apply entry-fn application args)
	(log4cl:log-warn "Entry function for ~A undefined" name))))

(defmethod configure-application ((application simple-application-mixin) &optional force-p)
  (with-slots (config-fn config-file name) application
    (when config-file
      (if (probe-file config-file)
	  (load config-file)
	  (log4cl:log-warn "Config file (~A) for ~A not found" file name)))
    (if config-fn
	(funcall config-fn application)
	(log4cl:log-warn "Config function for ~A undefined" name))))



;;;
;;; Simple CL Appplication
;;;

(defclass simple-cl-application-mixin (simple-application-mixin)
  ((loading-fn :initarg :loading-fn
	       :accessor application-loading-fn
	       :initform nil)))

(defmethod load-application ((application simple-cl-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (loading-fn name) application
    (if loading-fn
	(apply loading-fn application)
	(log4cl:log-warn "Loading function for ~A undefined" name))))

;;;
;;; Simple Shell
;;;

(defclass simple-shell-application-mixin ()
  ((make-command-fn :initarg :make-command-fn
		    :accessor shell-application-make-command-fn
		    :initform nil)))

(defmethod run-application ((application simple-shell-application-mixin) &rest args)
  (with-slots (make-command-fn name) application
    (if make-command-fn
	(uiop:run-program (apply make-command-fn args))
	(log4cl:log-warn "Make command function for ~A undefined" name))))
