(in-package :mcclim-desktop)

(defvar *manager* nil "The manager")

(defparameter *applications* (make-hash-table :test #'equal)
  "All applications")

;;;
;;; Abstract classes
;;;

(defclass manager ()
  ())

(defgeneric add-application-1 (manager application))
(defgeneric make-application-1 (manager name type args))

(defun add-application (name type &rest args)
  (add-application-1 *manager*
		     (make-application-1 *manager* name 'mcclim-application args)))

(defmethod make-application-1 ((manager manager) name type args)
  (let ((real-type (intern  (concatenate 'string "STANDARD-" (symbol-name type))
			    (find-package 'mcclim-desktop))))
    (apply #'make-instance real-type :name name args)))

(defmethod add-application-1 ((manager manager) application)
  (with-slots (name) application
    (setf (gethash name *applications*) application)))

(defun edit-file (filename)
  (let ((climacs-app (gethash "climacs" *applications*)))
    (configure-application climacs-app)
    (eval (read-from-string (format nil "(climacs:edit-file ~S)" filename)))))
