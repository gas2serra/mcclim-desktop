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
		       :type (member :boot :require :run))))

;;;
;;; Application protocols
;;;

(defgeneric run-application (application &rest args))
(defgeneric launch-application (application &key end-cb args))
(defgeneric note-application-start-running (application &rest args))
(defgeneric note-application-end-running (application &rest args))

(defgeneric configure-application (application &optional force-p))
(defgeneric note-application-configured (application))

;;; protocol: launch/running

(defmethod launch-application ((application application) &key end-cb args)
  (with-slots (name) application
    (bt:make-thread
     #'(lambda ()
	 (apply #'run-application application args)
	 (when end-cb
	   (funcall end-cb application :args args)))
     :name name)))

(defmethod run-application :around ((application application) &rest args)
  (let ((*application* application))
    (call-next-method)))

(defmethod run-application :before ((application application) &rest args)
  (declare (ignore args))
  (with-slots (configuration-time) application
    (case configuration-time
      (:require
       (configure-application application))
      (:run
       (configure-application application t))))
  (note-application-start-running application args))

(defmethod run-application :after ((application application) &rest args)
  (note-application-end-running application args))

(defmethod note-application-start-running ((application application) &rest args)
  (declare (ignore args)))

(defmethod note-application-end-running ((application application) &rest args)
   (declare (ignore args)))

;;; protocol: configure  

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

;;; protocol: initialization

(defmethod initialize-instance :after ((application application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name pretty-name configuration-time) application
    (unless pretty-name
      (setf pretty-name name))
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
   (loaded-p :initarg :loaded-p
	     :reader application-loaded-p
	     :initform nil)
   (loading-time :initarg :loading-time
		 :accessor application-loading-time
		 :initform :configuration
		 :type (member :boot :configuration))))

;;;
;;; protocols
;;;

(defgeneric load-application (application &optional force-p))
(defgeneric note-application-loaded (application))

;;; protocol: running

(defmethod run-application :around ((application cl-application) &rest args)
  (with-slots (debug-p) application
    (if debug-p
	(let ((*debugger-hook* *debugger-fn*))
	  (call-next-method))
	(call-next-method))))

;;; protocol: configure

(defmethod configure-application :before ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (loaded-p loading-time) application
    (when (or (eql loading-time :configuration) (not loaded-p))
      (load-application application))))

;;; protocol: Loading

(defmethod load-application :around ((application cl-application) &optional (force-p nil))
  (with-slots (loaded-p) application
    (when (or force-p (not loaded-p))
      (let ((*application* application))
	(call-next-method)))))

(defmethod load-application :before ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (system-name debug-system-p name) application
    (if system-name
	(if debug-system-p
	    (asdf:operate 'asdf:load-source-op system-name)
	    (asdf:load-system system-name))
	(log4cl:log-warn "System name for ~A undefined" name))))

(defmethod load-application :after ((application cl-application) &optional (force-p nil))
  (declare (ignore force-p))
  (with-slots (loaded-p) application
    (setf loaded-p t))
  (note-application-loaded application))

(defmethod note-application-loaded ((application cl-application))
  )

;; protocol: initialization
(defmethod initialize-instance :after ((application cl-application) &rest initargs)
  (declare (ignore initargs))
  (with-slots (loading-time) application
    (when (eql loading-time :boot)
      (load-application application))))

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
