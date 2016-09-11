(in-package :desktop-internals)

;;;;
;;;; Logger
;;;;

(defvar *logger* nil
  "The current logger")

;;;
;;; class
;;;

(defclass logger ()
  ((lock :initform (clim-sys:make-lock "logger"))))

;;;
;;; logger protocols
;;;

(defgeneric logger-log-info (logger msg))
(defgeneric logger-log-warn (logger msg))
(defgeneric logger-log-error (logger msg))

;;; methods
(defmethod logger-log-info :around ((logger logger) msg)
  (declare (ignore msg))
  (with-slots (lock) logger
    (clim-sys:with-lock-held (lock)
      (call-next-method))))

(defmethod logger-log-warn :around ((logger logger) msg)
  (declare (ignore msg))
  (with-slots (lock) logger
    (clim-sys:with-lock-held (lock)
      (call-next-method))))

(defmethod logger-log-error :around ((logger logger) msg)
  (declare (ignore msg))
  (with-slots (lock) logger
    (clim-sys:with-lock-held (lock)
      (call-next-method))))

;;;
;;; simple functions
;;;

(defun log-info (msg)
  (logger-log-info *logger* msg))

(defun log-warn (msg)
  (logger-log-warn *logger* msg))

(defun log-error (msg)
  (logger-log-error *logger* msg))

;;;
;;; mixin
;;;

(defclass stream-logger-mixin (logger)
  ((stream :initarg :stream
	   :accessor logger-stream
	   :initform *trace-output*)))

(defmethod logger-log-info ((logger stream-logger-mixin) msg)
  (with-slots (stream) logger
    (format stream "Info: ~A~%" msg)))

(defmethod logger-log-warn ((logger stream-logger-mixin) msg)
  (with-slots (stream) logger
    (format stream "Warn: ~A~%" msg)))

(defmethod logger-log-error ((logger stream-logger-mixin) msg)
  (with-slots (stream) logger
    (format stream "Error: ~A~%" msg)))

;;;
;;; standard class
;;;

(defclass standard-logger (stream-logger-mixin logger)
  ())

(defun make-logger (type &rest args)
  (setf *logger* (apply #'make-instance type args)))
