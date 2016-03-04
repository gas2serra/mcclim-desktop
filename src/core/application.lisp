(in-package :cl-desktop)

;;;;
;;;; Abstract Application classes
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
   (configured-p :reader application-configured-p
		 :initform nil)
   (configuration-time :initarg :configuration-time
		       :accessor application-configuration-time
		       :initform :require
		       :type (member :boot :require :run))
   (loading-time :initarg :loading-time
		       :accessor application-loading-time
		       :initform :require
		       :type (member :boot :require :run))
   (loaded-p :reader application-loaded-p
	     :initform nil)))

;;;
;;; Application protocols
;;;

(defgeneric run-application (application &rest args))
(defgeneric launch-application (application &key end-cb args))
(defgeneric note-application-start-running (application &rest args))
(defgeneric note-application-end-running (application &rest args))

(defgeneric configure-application (application &optional force-p))
(defgeneric note-application-configured (application))

(defgeneric load-application (application &optional force-p))
(defgeneric note-application-loaded (application))

;;; protocol: launch/running

(defmethod launch-application ((application application) &key end-cb args)
  (with-slots (name) application
    (clim-sys:make-process 
     #'(lambda ()
	 (unwind-protect
	      (apply #'run-application application args)
	   (when end-cb
	     (funcall end-cb application :args args))))
     :name name)))

(defmethod run-application :around ((application application) &rest args)
  (let ((*application* application))
    (note-application-start-running application args)
    (unwind-protect
	 (call-next-method)
      (note-application-end-running application args))))

(defmethod run-application :before ((application application) &rest args)
  (declare (ignore args))
  (with-slots (configuration-time) application
    (case configuration-time
      (:require
       (configure-application application))
      (:run
       (configure-application application t)))))

(defmethod note-application-start-running ((application application) &rest args)
  (declare (ignore args)))

(defmethod note-application-end-running ((application application) &rest args)
   (declare (ignore args)))

;;; protocol: configure

(defmethod configure-application :before ((application application) &optional (force-p nil))
  (declare (ignore args))
  (with-slots (loading-time) application
    (case loading-time
      (:require
       (load-application application))
      (:run
       (load-application application t)))))

(defmethod configure-application :around ((application application) &optional (force-p nil))
  (with-slots (configured-p) application
    (when (or force-p (not configured-p))
      (let ((*application* application))
	(call-next-method)))))

(defmethod configure-application :after ((application application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (configured-p) application
    (setf configured-p t))
  (note-application-configured application))

(defmethod note-application-configured ((application application))
  )

;;; protocol: loading

(defmethod load-application :around ((application application) &optional (force-p nil))
  (with-slots (loaded-p) application
    (when (or force-p (not loaded-p))
      (let ((*application* application))
	(call-next-method)))))

(defmethod load-application :after ((application application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (loaded-p) application
    (setf loaded-p t))
  (note-application-loaded application))

(defmethod note-application-loaded ((application application))
  )


;;; protocol: initialization

(defmethod initialize-instance :after ((application application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name pretty-name loading-time configuration-time) application
    (unless pretty-name
      (setf pretty-name name))
    (when (eql loading-time :boot)
      (load-application application))
    (when (eql configuration-time :boot)
      (configure-application application))))

;;;
;;; CL Application
;;;

(defclass cl-application (application)
  ((debug-p :initarg :debug-p
	    :accessor application-debug-p
	    :initform t)
   (system-name :initarg :system-name
		:accessor application-system-name
		:initform nil)
   (debug-system-p :initarg :debug-system-p
		   :accessor application-debug-system-p
		   :initform nil)
   (system-loaded-p :initarg :system-loaded-p
		    :reader application-system-loaded-p
		    :initform nil)))

;;;
;;; protocols
;;;

(defgeneric load-application-system (application &optional force-p))
(defgeneric note-application-system-loaded (application))

;;; protocol: running

(defmethod run-application :around ((application cl-application) &rest args)
  (with-slots (debug-p) application
    (let ((*debugger-hook* (debugger-hook debug-p)))
      (call-next-method))))

;;; protocol: configure

(defmethod configure-application :before ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-loaded-p) application
    (unless system-loaded-p
      (load-application-system application))))

;;; protocol: Loading

(defmethod load-application :before ((application cl-application) &optional (force-p nil))
  (load-application-system application force-p))

(defmethod load-application-system :around ((application cl-application) &optional (force-p nil))
  (with-slots (system-loaded-p) application
    (when (or force-p (not system-loaded-p))
      (let ((*application* application))
	(call-next-method)))))

(defmethod load-application-system :before ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-name debug-system-p name) application
    (if system-name
	(if debug-system-p
	    (asdf:operate 'asdf:load-source-op system-name :force-not t)
	    (asdf:operate 'asdf:load-op system-name))
	(log-warn (format nil "System name for ~A undefined" name)))))

(defmethod load-application-system :after ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-loaded-p) application
    (setf system-loaded-p t))
  (note-application-system-loaded application))

(defmethod note-application-system-loaded ((application cl-application))
  )

;; protocol: initialization
(defmethod initialize-instance :after ((application cl-application) &rest initargs)
  (declare (ignore initargs)))

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

(defmethod configure-application ((application alias-application) &optional force-p)
  (with-slots (reference) application
    (configure-application reference force-p)))

(defmethod launch-application ((application alias-application) &key end-cb args)
  (with-slots (reference) application
    (launch-application reference :end-cb end-cb :args args)))

(defmethod run-application ((application alias-application) &rest args)
  (with-slots (reference) application
    (apply #'run-application reference args)))


(defclass proxy-application (link-application)
  ())

;;;
;;; Shell Application
;;;

(defclass shell-application (application)
  ())

(defmethod configure-application ((application shell-application) &optional force-p)
  (declare (ignore force-p)))


;;;
;;; Utility Function
;;;

(defun make-application (name type &rest args)
  (apply #'make-instance type :name name args))
