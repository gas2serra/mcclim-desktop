(in-package :desktop-internals)

;;;;
;;;; Application Mixin
;;;; 

;;;
;;; Simple Application Mixin
;;;

(defclass simple-application-mixin ()
  ((entry-fn :initarg :entry-fn
	     :accessor application-entry-fn
	     :initform nil)))

(defmethod run-application ((application simple-application-mixin) &rest args)
  (with-slots (entry-fn name) application
    (if entry-fn
	(apply entry-fn application args)
	(log-warn (format nil "Entry function for ~A undefined" name)))))

;;;
;;; Simple CL Appplication Mixin
;;;

(defclass simple-cl-application-mixin (simple-application-mixin)
  ())
   
(defmethod load-application ((application simple-cl-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (name system-name debug-system-p) application
    (if system-name
	(if debug-system-p
	    (asdf:operate 'asdf:load-source-op system-name :force-not t)
	    (asdf:require-system system-name))
	(log-warn (format nil "System name for ~A undefined" name)))))

(defmethod install-application ((application simple-cl-application-mixin) &optional force-p)
  (declare (ignore force-p))
  (with-slots (name system-name git-repo) application
    (if system-name
	(handler-case 
	    (ql:quickload system-name)
	  (ql:system-not-found ()
	    (log-warn (format nil "System ~A non found in quicklisp" name))
	    (if git-repo
		(let ((cur-dir (uiop/os:getcwd)))
		  (uiop/os:chdir (first ql:*local-project-directories*))
		  (uiop/run-program:run-program
		   (list "git" "clone" git-repo)
		   :force-shell t :output t :error-output t)
		  (uiop/os:chdir cur-dir)
		  (ql:quickload system-name))
		(log-warn (format nil "Git repository for ~A undefined" name)))))
	(log-warn (format nil "System name for ~A undefined" name)))))

;;;
;;; Simple Shell Mixin
;;;

(defclass simple-shell-application-mixin (simple-application-mixin)
  ((make-command-fn :initarg :make-command-fn
		    :accessor application-make-command-fn
		    :initform nil))
  (:default-initargs
   :entry-fn #'(lambda (application &rest args)
		 (with-slots (make-command-fn name) application
		   (if make-command-fn
		       (uiop:run-program (apply make-command-fn args))
		       (log-warn (format nil "Make command function for ~A undefined" name)))))))

;;;;
;;;; Standard Application Mixin
;;;;

(defclass standard-application-mixin ()
  ())

;;; protocols

(defgeneric application-file (application &optional force-p force-user-p))
(defgeneric application-config-file (application &optional force-p force-user-p))
(defgeneric application-style-file (application &optional force-p force-user-p force-style))

(defgeneric load-application-config-file (application))
(defgeneric load-application-style-file (application &optional force-style))

;;; protocol: application files

(defmethod application-file ((application standard-application-mixin)
			     &optional force-p force-user-p)
  (with-slots (name) application
    (find-application-file name force-p force-user-p)))
	
(defmethod application-config-file ((application standard-application-mixin)
				    &optional force-p force-user-p)
  (with-slots (name) application
    (find-config-file name nil force-p force-user-p)))

(defmethod application-style-file ((application standard-application-mixin)
				   &optional force-p force-user-p force-style)
  (with-slots (name style) application
    (find-config-file name (or force-style style *application-style*)
				 force-p force-user-p)))

;;; protocol: load files

(defmethod load-application-config-file ((application standard-application-mixin))
  (with-slots (name) application
    (let ((config-file (application-config-file application)))
      (if config-file
	  (let ((*application* application))
	    (load config-file))
	  (log-warn (format nil "Config file for ~A not found" name))))))

(defmethod load-application-style-file ((application standard-application-mixin)
					&optional force-style)
  (with-slots (name style) application
    (let ((sty (or force-style style *application-style*)))
      (let ((style-file (application-style-file application nil nil sty)))
	(if style-file
	    (let ((*application* application))
	      (load style-file))
	    (progn
	      (log-warn (format nil "Style file (~A) for ~A not found" sty name))
	      (unless (eq sty :default)
		(load-application-style-file application :default))))))))

;;; protocol: configure

(defmethod configure-application :before ((application standard-application-mixin)
					    &optional force-p)
  (declare (ignore force-p))
  (load-application-config-file application)
  (load-application-style-file application))
    
;;;
;;; Standard CL Application Mixin
;;;

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())
