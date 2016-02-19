(in-package :mcclim-desktop)

;;;
;;; Abstract classes
;;;

;; Application

(defclass application ()
  ((name :initarg :name
	 :reader application-name
	 :initform "")
   (pretty-name :initarg :pretty-name
		:accessor application-pretty-name
		:initform nil)
   (configured-p :reader application-configured-p
		 :initform nil)
   (configuration-time :initarg :configuration-time
		       :accessor application-configuration-time
		       :initform :require
		       :type (member :boot :require :run))))

(defgeneric configure-application (application &optional force-p))
(defgeneric run-application (application &rest args))
(defgeneric note-starting-application (application &rest args))
(defgeneric note-ending-application (application &rest args))

(defmethod configure-application :after ((application application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (configured-p) application
    (setf configured-p t)))

(defmethod configure-application :around ((application application) &optional (force-p nil))
  (with-slots (configured-p) application
    (when (or force-p (not configured-p))
      (call-next-method))))

(defmethod run-application :before ((application application) &rest args)
  (with-slots (configuration-time) application
    (case configuration-time
      (:require
       (configure-application application))
      (:run
       (configure-application application t)))))

(defmethod note-starting-application ((application application) &rest args)
  )

(defmethod note-ending-application ((application application) &rest args)
  )

(defmethod initialize-instance :after ((application application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name pretty-name configuration-time) application
    (unless pretty-name
      (setf pretty-name name))
    (when (eql configuration-time :boot)
      (configure-application application))))

;; CL application

(defclass cl-application (application)
  ((debug-p :initarg :debug-p
	    :accessor application-debug-p
	    :initform t)
   (system-name :initarg :system-name
		:accessor application-system-name
		:initform nil)
   (loaded-p :initarg :loaded-p
	     :reader application-loaded-p
	     :initform nil)
   (loading-time :initarg :loading-time
		 :accessor application-loading-time
		 :initform :configuration
		 :type (member :boot :configuration))))

(defgeneric load-application (application &optional force-p))

(defmethod load-application :after ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (loaded-p) application
    (setf loaded-p t)))

(defmethod load-application :around ((application cl-application) &optional (force-p nil))
  (with-slots (loaded-p) application
    (when (or force-p (not loaded-p))
      (call-next-method))))

(defmethod load-application ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-name name) application
    (if system-name
	(asdf:operate 'asdf:load-op system-name)
	(warn "System name for ~A undefined" name))))

(defmethod configure-application :before ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (loaded-p loading-time) application
    (when (or (eql loading-time :configuration) (not loaded-p))
      (load-application application))))

(defmethod initialize-instance :after ((application cl-application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (system-name loading-time) application
    (when (eql loading-time :boot)
      (load-application application))))


;; McClim Application

(defclass mcclim-application (cl-application)
  ((frame-class :initarg :frame-class
		:accessor application-frame-class
		:initform nil)))

;; Alias
(defclass alias-application (application)
  ((to :initarg :to
       :initform nil
       :accessor application-alias-to)))

(defmethod configure-application ((application alias-application) &optional force-p)
  (with-slots (to) application
    (configure-application to force-p)))

(defmethod run-application ((application alias-application) &rest args)
  (with-slots (to) application
    (apply #'run-application to args)))

;; Link
(defclass link-application (application)
  ((to :initarg :to
       :initform nil
       :accessor application-link-to)))

;; external

(defclass shell-application (application)
  ((command :initarg :command
	    :initform nil
	    :accessor application-command)))

(defmethod configure-application ((application shell-application) &optional force-p)
  )

(defmethod run-application ((application shell-application) &rest args)
  (with-slots (command name) application
    (if command
	(bt:make-thread
	 #'(lambda ()
	     (note-starting-application application args)
	     (uiop:run-program (format nil command args))
	     (note-ending-application application args))
	 :name name)
	(warn "Entry function for ~A undefined" name))))


;;;
;;; Standard application mixin
;;;

(defvar *application* nil "The current application")

(defclass standard-application-mixin ()
  ((entry-fn :initarg :entry-fn
	     :accessor application-entry-fn
	     :initform nil)
   (config-file :initarg :config-file
		:accessor application-config-file
		:initform nil)))

(defmethod run-application ((application standard-application-mixin) &rest args)
  (with-slots (entry-fn debug-p name) application
    (if entry-fn
	(bt:make-thread
	 #'(lambda ()
	     (note-starting-application application args)
	     (apply entry-fn application args)
	     (note-ending-application application args))
	 :name name)
	(warn "Entry function for ~A undefined" name))))

(defmethod configure-application ((application standard-application-mixin) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (name configuration-type config-file) application
    (let ((*application* application))
      (if config-file
	  (if (probe-file config-file)
	      (load config-file)
	      (error "System config (~A) for ~A not found" config-file name))
	  (load-default-config-file name)))))

(defclass standard-cl-application-mixin (standard-application-mixin)
  ())

(defmethod run-application ((application standard-cl-application-mixin) &rest args)
  (with-slots (entry-fn debug-p name) application
    (if entry-fn
	(bt:make-thread
	 (if debug-p
	     #'(lambda ()
		 (let ((*debugger-hook* *debugger-fn*)
		       (*application* application))
		   (note-starting-application application args)
		   (apply entry-fn application args)
		   (note-ending-application application args)))
	     #'(lambda ()
		 (note-starting-application application args)
		 (apply entry-fn args)
		 (note-ending-application application args)))
	 :name name)
	(warn "Entry function for ~A undefined" name))))


;;;
;;; Standard application
;;;

(defclass standard-cl-application (standard-cl-application-mixin cl-application)
  ())

(defclass standard-mcclim-application (standard-cl-application-mixin mcclim-application)
  ())
   
(defclass standard-alias-application (alias-application)
  ())

(defclass standard-link-application (standard-application-mixin link-application)
  ())

(defclass standard-shell-application (shell-application)
  ())
