(in-package :mcclim-desktop-core)

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
(defgeneric application-style-file (application &optional force-p force-user-p))
(defgeneric application-default-style-file (application &optional force-p force-user-p))

;;; protocol: application files

(defmethod application-file ((application standard-application-mixin)
			     &optional force-p force-user-p)
  (with-slots (name) application
    (find-application-file name force-p force-user-p)))
	
(defmethod application-config-file ((application standard-application-mixin)
				    &optional force-p force-user-p)
  (with-slots (name) application
    (find-application-config-file name force-p force-user-p)))

(defmethod application-style-file ((application standard-application-mixin)
				   &optional force-p force-user-p)
  (with-slots (name style) application
    (find-application-style-file name (or style *application-style*) force-p force-user-p)))

(defmethod application-default-style-file ((application standard-application-mixin)
					   &optional force-p force-user-p)
  (with-slots (name) application
    (find-application-style-file name :default force-user-p)))
  

;;; protocol: application config file

(defmethod configure-application :before ((application standard-application-mixin)
					    &optional force-p)
  (declare (ignore force-p))
  (with-slots (name style) application
    (let ((config-file (application-config-file application)))
      (if config-file
	  (let ((*application* application))
	    (load config-file))
	  (log-warn (format nil "Config file (~A) for ~A not found"
			    (application-relative-config-file-pathname name) name))))
    (let ((style-file (application-style-file application)))
      (if style-file
	  (let ((*application* application))
	    (load style-file))
	  (progn
	    (log-warn (format nil "Style file (~A) for ~A not found"
			      (application-relative-style-file-pathname name (or style *application-style*)) name))
	    (unless (eq style-file :default)
	      (let ((style-file (application-default-style-file application)))
		(if style-file
		    (let ((*application* application))
		      (load style-file))
		    (log-warn (format nil "Style file (~A) for ~A not found"
				      (application-relative-style-file-pathname name :default) name))))))))))
    
;;;
;;; Standard CL Application Mixin
;;;

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())
