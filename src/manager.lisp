(in-package :mcclim-desktop)

(defvar *manager* nil "The manager")

(defparameter *applications* (make-hash-table :test #'equal)
  "All applications")

;;;
;;; Abstract classes
;;;

(defclass manager ()
  ((applications :initform (make-hash-table :test #'equal)
		 :reader manager-applications)
   (processes :initform nil
	      :reader manager-processes)))

(defgeneric get-application (manager name))
(defgeneric find-application (manager name))
(defgeneric add-application-1 (manager application))
(defgeneric make-application-1 (manager name type args))

(defmethod get-application ((manager manager) name)
  (with-slots (applications) manager
    (gethash name applications)))

(defmethod find-application ((manager manager) name)
  (let ((application (get-application manager name)))
    (unless application
      (load-default-application-file name)
      (setf application (get-application manager name)))
    application))
	    

(defmethod make-application-1 ((manager manager) name type args)
  (let ((real-type (intern  (concatenate 'string "STANDARD-" (symbol-name type))
			    (find-package 'mcclim-desktop))))
    (apply #'make-instance real-type :name name args)))

(defmethod add-application-1 ((manager manager) application)
  (with-slots (name) application
    (with-slots (applications) manager
      (setf (gethash name applications) application))))

(defun edit-file (filename)
  (let ((editor (find-application *manager* "editor")))
    (run-application editor filename)))

(defun add-application (name type &rest args)
  (add-application-1 *manager* (make-application-1 *manager* name type args)))

(defmethod note-starting-application ((application application) &rest args)
  (with-slots (processes) *manager*
    (push (list application args (bt:current-thread)) processes)))

(defmethod note-ending-application ((application application) &rest args)
  (with-slots (processes) *manager*
    (setf processes (delete-if #'(lambda (x) (equal (bt:current-thread) x))
			       processes
			       :key #'third))))
