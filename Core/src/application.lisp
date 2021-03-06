(in-package :desktop-internals)

;;;;
;;;; Global Variables
;;;;

(defvar *application* nil
  "The current application")

;;;;
;;;; Application Classes
;;;;

;;;
;;; Application
;;;

(defclass application ()
  ((name :initarg :name
	 :reader application-name)
   (pretty-name :initarg :pretty-name
		:accessor application-pretty-name
		:initform nil)
   (icon :initarg :icon
	 :accessor application-icon
	 :initform nil)
   (menu-p :initarg :menu-p
	   :initform t
	   :accessor application-menu-p)
   (requires-args-p :initarg :requires-args-p
		    :initform nil
		    :accessor application-requires-args-p)
   (configured-p :reader application-configured-p
		 :initform nil)))

;;;
;;; Application protocols
;;;

(defgeneric run-application (application &rest args))
(defgeneric launch-application (application &key args cb-fn))
(defgeneric note-application-start-running (application &rest args))
(defgeneric note-application-end-running (application &rest args))

(defgeneric configure-application (application &optional force-p))
(defgeneric ensure-application-configured (application))
(defgeneric need-reconfigure-application (application))
(defgeneric note-application-configured (application))

;;; protocol: launch/running

(defmethod launch-application ((application application) &key args cb-fn)
  (with-slots (name) application
    (bt:make-thread 
     #'(lambda ()
	 (unwind-protect
	      (let ((res (apply #'run-application application args)))
		(when cb-fn
		  (funcall cb-fn res application args)))))
     :name name)))

(defmethod run-application :around ((application application) &rest args)
  (ensure-application-configured application)
  (note-application-start-running application args)
  (unwind-protect
       (call-next-method)
    (note-application-end-running application args)))

(defmethod note-application-start-running ((application application) &rest args)
  (declare (ignore args))
  (with-slots (name) application
    (log-info (format nil "Start running ~A application" name))))

(defmethod note-application-end-running ((application application) &rest args)
  (declare (ignore args))
  (with-slots (name) application
    (log-info (format nil "End runnig ~A application" name))))

;;; protocol: configure

(defmethod configure-application ((application application) &optional (force-p nil))
  (declare (ignore application force-p)))

(defmethod configure-application :around ((application application) &optional (force-p nil))
  (with-slots (configured-p) application
    (when (or force-p (not configured-p))
      (call-next-method)
      (setf configured-p t)
      (note-application-configured application))))

(defmethod ensure-application-configured ((application application))
  (with-slots (configured-p) application
    (when (not configured-p)
      (configure-application application))))

(defmethod need-reconfigure-application ((application application))
  (with-slots (configured-p) application
    (setf configured-p nil)))

(defmethod note-application-configured ((application application))
  (with-slots (name) application
    (log-info (format nil "Configured ~A application" name))))

;;; initialize

(defmethod initialize-instance :after ((application application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name pretty-name) application
    (unless pretty-name
      (setf pretty-name name))))

;;; print-object

(defmethod print-object ((obj application) stream)
   (print-unreadable-object (obj stream :type t :identity t)
     (princ (application-name obj) stream)))

;;;
;;; CL Application
;;;

(defclass cl-application (application)
  ((home-page :initarg :home-page
	      :accessor application-home-page
	      :initform nil)
   (git-repo :initarg :git-repo
	     :accessor application-git-repo
	     :initform nil)
   (system-name :initarg :system-name
		:accessor application-system-name
		:initform nil)
   (debug-system-p :initarg :debug-system-p
		   :accessor application-debug-system-p
		   :initform nil)
   (loaded-p :reader application-loaded-p
	     :initform nil)
   (installed-p :reader application-installed-p
		:initform nil)))

;;;
;;; protocols
;;;

(defgeneric load-application (application &optional force-p))
(defgeneric ensure-application-loaded (application))
(defgeneric need-reload-application (application))
(defgeneric note-application-loaded (application))

(defgeneric install-application (application &optional force-p))
(defgeneric ensure-application-installed (application))
(defgeneric need-reinstall-application (application))
(defgeneric note-application-installed (application))

;;; protocol: running

(defmethod run-application :around ((application cl-application) &rest args)
  (declare (ignore args))
  (swank/backend:call-with-debugger-hook
   #'debugger-hook
   (lambda ()
     (call-next-method))))

;;; protocol: config

(defmethod configure-application :around ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (ensure-application-loaded application)
  (call-next-method))

;;; protocol: loading

(defmethod load-application :around ((application cl-application) &optional (force-p nil))
  (ensure-application-installed application)
  (with-slots (loaded-p) application
    (when (or force-p (not loaded-p))
      (call-next-method)
      (setf loaded-p t)
      (need-reconfigure-application application)
      (note-application-loaded application))))

(defmethod ensure-application-loaded ((application cl-application))
  (with-slots (loaded-p) application
    (when (not loaded-p)
      (load-application application))))

(defmethod need-reload-application ((application cl-application))
  (with-slots (loaded-p) application
    (setf loaded-p nil))
  (need-reconfigure-application application))

(defmethod note-application-loaded ((application cl-application))
  (with-slots (name) application
    (log-info (format nil "Loaded ~A application" name))))

;;; protocol: installing

(defmethod install-application :around ((application cl-application) &optional (force-p nil))
  (with-slots (installed-p system-name) application
    (when (or force-p (not installed-p))
      (unless (asdf:find-system system-name nil)
	(call-next-method))
      (setf installed-p t)
      (need-reload-application application)
      (note-application-installed application))))

(defmethod ensure-application-installed ((application cl-application))
  (with-slots (installed-p) application
    (when (not installed-p)
      (install-application application))))

(defmethod need-reinstall-application ((application cl-application))
  (with-slots (installed-p) application
    (setf installed-p nil))
  (need-reload-application application))

(defmethod note-application-installed ((application cl-application))
  (with-slots (name) application
    (log-info (format nil "Installed ~A application" name))))

;;;
;;; McClim Application
;;;

(defclass mcclim-application (cl-application)
  ((frame-class :initarg :frame-class
		:accessor application-frame-class
		:initform nil)))    

;;;
;;; Link/Alias/Proxy Applications
;;;

(defclass link-application (application)
  ((reference :initarg :reference
	      :initform nil
	      :accessor application-link-reference)))

(defclass alias-application (link-application)
  ())

(defmethod launch-application ((application alias-application) &key args cb-fn)
  (with-slots (reference) application
    (funcall #'launch-application reference :args args :cb-fn cb-fn)))

(defmethod run-application ((application alias-application) &rest args)
  (with-slots (reference) application
    (apply #'run-application reference args)))

(defmethod configure-application ((application alias-application) &optional force-p)
  (with-slots (reference) application
    (configure-application reference force-p)))

(defclass proxy-application (link-application)
  ())

;;;
;;; Shell Application
;;;

(defclass shell-application (application)
  ())
