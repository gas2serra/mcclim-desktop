(in-package :cl-desktop)

;;;;
;;;; Abstract Manager Class
;;;;

(defclass manager ()
  ((name->application :initform (make-hash-table :test #'equal))
   (force-debug-p :initarg :force-debug-p
		  :accessor manager-force-debug-p
		  :initform nil)
   (configured-p :reader manager-configured-p
		 :initform nil)))

;;;
;;; protocols 
;;;

(defgeneric get-application-1 (manager name))
(defgeneric find-application-1 (manager name))
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

(defmethod get-application-1 ((manager manager) name)
  (with-slots (name->application) manager
    (gethash name name->application)))

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

(defmethod configure-manager :around ((manager manager) &optional (force-p nil))
  (with-slots (configured-p) manager
    (when (or force-p (not configured-p))
      (call-next-method)
      (setf configured-p t)
      (note-manager-configured manager))))

(defmethod note-manager-configured ((manager manager))
  (log-info (format nil "configured manager")))

;;;
;;; Utility functions
;;;

(defun get-application (name &optional (manager *manager*))
  (get-application-1 manager name))

(defun find-application (name &optional (manager *manager*))
  (find-application-1 manager name))

(defun register-application (name type &rest args)
  (add-application-1 *manager*
   (apply #'make-application name type args)))

(defun debugger-hook (debug-p &optional (manager *manager*))
  (manager-debugger-hook manager debug-p))

(defun log-info (msg &optional (manager *manager*))
  (manager-log-info manager msg))

(defun log-warn (msg &optional (manager *manager*))
  (manager-log-warn manager msg))
