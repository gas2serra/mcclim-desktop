(in-package :cl-desktop)

;;;;
;;;; Abstract Manager Class
;;;;

(defclass manager ()
  ((name->application :initform (make-hash-table :test #'equal)
		      :reader manager-name->application)
   (thread->process-info :initform (make-hash-table :test #'eql)
			 :reader manager-thread->process-info)))

;;;
;;; protocols 
;;;

(defgeneric get-application-1 (manager name))
(defgeneric find-application-1 (manager name))
(defgeneric add-application-1 (manager application))
(defgeneric note-manager-add-application (manager application))

(defgeneric manager-debugger-hook (manager debug-p))
(defgeneric manager-log-warn (manager msg))

(defgeneric manager-setup (manager))

(defgeneric refresh-applications (manager))

;;; protolog: get

(defmethod get-application-1 ((manager manager) name)
  (with-slots (name->application) manager
    (gethash name name->application)))

;;; protocol add

(defmethod add-application-1 ((manager manager) application)
  (with-slots (name) application
    (with-slots (name->application) manager
      (setf (gethash name name->application) application))))

(defmethod note-manager-add-application ((manager manager) application)
  )

;;; protocol: application runnig

(defmethod note-application-start-running ((application application) &rest args)
  (with-slots (thread->process-info) *manager*
    (setf (gethash (bt:current-thread) thread->process-info) 
	  (list application args))))

(defmethod note-application-end-running ((application application) &rest args)
  (declare (ignore args))
  (with-slots (thread->process-info) *manager*
    (remhash (bt:current-thread) thread->process-info)))

;;; refresh

(defmethod refresh-applications ((manager manager))
  (with-slots (name->application) manager
    (maphash #'(lambda (k v)
		 (configure-application v t))
	     name->application)))

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

(defun log-warn (msg &optional (manager *manager*))
  (manager-log-warn manager msg))
