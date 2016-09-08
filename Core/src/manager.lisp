(in-package :mcclim-desktop-core)

;;;;
;;;; Abstract Manager Class
;;;;

(defclass manager ()
  ((name->application :initform (make-hash-table :test #'equal))
   (debugger-fn :initarg :debugger-fn
		:accessor manager-debugger-fn
		:initform nil)
   (configured-p :reader manager-configured-p
		 :initform nil)
   (log-lock :initform (clim-sys:make-lock "manager-log"))))

;;;
;;; protocols 
;;;

(defgeneric get-application-1 (manager name &optional errorp))
(defgeneric find-application-1 (manager name &optional errorp))
(defgeneric add-application-1 (manager application))
(defgeneric note-manager-added-application (manager application))

(defgeneric configure-manager (manager &optional force-p))
(defgeneric note-manager-configured (manager))

(defgeneric manager-debugger-hook (manager debug-p))
(defgeneric manager-log-info (manager msg))
(defgeneric manager-log-warn (manager msg))

(defgeneric get-applications (manager))
(defgeneric map-applications (manager fn))

;;; protocol: get/add

(defmethod get-application-1 ((manager manager) name &optional (errorp t))
  (with-slots (name->application) manager
    (or (gethash name name->application)
	(and errorp (error "Application ~A not found" name)))))

(defmethod add-application-1 ((manager manager) application)
  (with-slots (name) application
    (with-slots (name->application) manager
      (setf (gethash name name->application) application)))
  (note-manager-added-application manager application))
    
(defmethod note-manager-added-application ((manager manager) application)
  (with-slots (name) application
    (log-info (format nil "added ~A application" name))))

;;; protocol: access applications

(defmethod get-applications ((manager manager))
  (with-slots (name->application) manager
    (alexandria:hash-table-values name->application)))

(defmethod map-applications ((manager manager) fn)
  (with-slots (name->application) manager
    (alexandria:maphash-values fn name->application)))

;;; protocol: configure

(defmethod configure-manager ((manager manager) &optional force-p)
  (declare (ignore manager force-p)))

(defmethod configure-manager :around ((manager manager) &optional (force-p nil))
  (with-slots (configured-p) manager
    (when (or force-p (not configured-p))
      (call-next-method)
      (setf configured-p t)
      (note-manager-configured manager))))

(defmethod note-manager-configured ((manager manager))
  (log-info (format nil "configured manager")))

;;; protocol: log

(defmethod manager-log-info :around ((manager manager) msg)
  (declare (ignore manager msg))
  (with-slots (log-lock) manager
    (clim-sys:with-lock-held (log-lock)
      (call-next-method))))

(defmethod manager-log-warn :around ((manager manager) msg)
  (declare (ignore manager msg))
  (with-slots (log-lock) manager
    (clim-sys:with-lock-held (log-lock)
      (call-next-method))))

;;;
;;; Utility functions
;;;

(defun get-application (name &optional (errorp t) (manager *manager*))
  (get-application-1 manager name errorp))

(defun find-application (name &optional (errorp t) (manager *manager*))
  (find-application-1 manager name errorp))

(defun register-application (name type &rest args)
  (add-application-1 *manager*
   (apply #'make-application name type args)))

(defun log-info (msg &optional (manager *manager*))
  (manager-log-info manager msg))

(defun log-warn (msg &optional (manager *manager*))
  (manager-log-warn manager msg))
