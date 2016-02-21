(in-package :cl-desktop)



(defclass manager ()
  ((applications :initform (make-hash-table :test #'equal)
		 :reader manager-applications)
   (processes :initform nil
	      :reader manager-processes)))

(defgeneric get-application-1 (manager name))
(defgeneric find-application-1 (manager name))
(defgeneric add-application-1 (manager application))

(defmethod get-application-1 ((manager manager) name)
  (with-slots (applications) manager
    (gethash name applications)))

(defmethod add-application-1 ((manager manager) application)
  (with-slots (name) application
    (with-slots (applications) manager
      (setf (gethash name applications) application))))

;;;
;;; non va bene...
;;;

(defmethod note-application-start-running ((application application) &rest args)
  (with-slots (processes) *manager*
    (push (list application args (bt:current-thread)) processes)))

(defmethod note-application-end-running ((application application) &rest args)
  (with-slots (processes) *manager*
    (setf processes (delete-if #'(lambda (x) (equal (bt:current-thread) x))
			       processes
			       :key #'third))))


;;;
;;;
;;;


		       
(defun get-application (name &optional (manager *manager*))
  (get-application-1 manager name))

(defun find-application (name &optional (manager *manager*))
  (find-application-1 manager name))

(defun make-application (name type &rest args)
  (apply #'make-instance type :name name args))

(defun register-application (name type &rest args)
  (add-application-1 *manager*
   (apply #'make-application name type args)))
  
