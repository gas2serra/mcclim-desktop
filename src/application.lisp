(in-package :mcclim-desktop)

;;;
;;; Abstract classes
;;;

;; Application

(defclass application ()
  ((name :initarg :name
	 :accessor application-name
	 :initform "")
   (pretty-name :initarg :pretty-name
		:accessor application-pretty-name
		:initform nil)
   (configured-p :reader application-configured-p
		 :initform nil)
   (configuration-type :accessor application-configuration-type
		       :initargs :configuration-type
		       :initform :both
		       :type (member :both :user :system))
   (configuration-time :initarg :configuration-time
		       :accessor application-configuration-time
		       :initform :require
		       :type (member :boot :require :run))))

(defgeneric configure-application (application &optional force-p))
(defgeneric run-application (application))

(defmethod configure-application :after ((application application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (configured-p) application
    (setf configured-p t)))

(defmethod configure-application :around ((application application) &optional (force-p nil))
  (with-slots (configured-p) application
    (when (or force-p (not configured-p))
      (call-next-method))))

(defmethod run-application :before ((application application))
  (with-slots (configuration-time) application
    (case configuration-time
      (:require
       (configure-application application))
      (:run
       (configure-application application t)))))
       
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
		:reader application-system-name
		:initform nil)
   (loaded-p :initarg :loaded-p
	     :accessor application-loaded-p
	     :initform nil)
   (loading-time :initarg :loading-time
		 :reader application-loading-time
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

;;;
;;; Standard application mixin
;;;

(defclass standard-application-mixin ()
  ((entry-fn :initarg :entry-fn
	     :accessor application-entry-fn
	     :initform nil)))

(defvar *application* nil "The current application")

(defmethod run-application ((application standard-application-mixin))
  (with-slots (entry-fn debug-p name) application
    (if entry-fn
	(bt:make-thread
	 (if debug-p
	     #'(lambda ()
		 (let ((*debugger-hook* *debugger-fn*)
		       (*application* application))
		   (funcall entry-fn)))
	    #'(lambda () (funcall entry-fn)))
	 :name name)
	(warn "Entry function for ~A undefined" name))))

(defmethod configure-application ((application standard-application-mixin) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (name configuration-type) application
  (let ((system-config-file (uiop:merge-pathnames*
			     (format nil "etc/~A.lisp" name)
			     *mcclim-desktop-directory*))
	(user-config-file (uiop:merge-pathnames*
			   (format nil "etc/~A.lisp" name)
			   *user-directory*)))
    (if (and (probe-file system-config-file) (member configuration-type '(:system :both)))
	(let ((*application* application))
	  (load system-config-file))
	(error "System config (~A) for ~A not found" system-config-file (application-name application)))
    (if (and (probe-file user-config-file) (member configuration-type '(:user :both)))
	(let ((*application* application))
	  (load user-config-file))
	(warn "User config (~A) for ~A not found" system-config-file (application-name application))))))


;;;
;;; Standard application
;;;

(defclass standard-cl-application (standard-application-mixin cl-application)
  ())


(defclass standard-mcclim-application (standard-application-mixin mcclim-application)
  ())
   
