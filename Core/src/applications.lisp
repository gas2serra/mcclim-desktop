(in-package :mcclim-desktop-core)

;;;;
;;;; Applications
;;;;

(defvar *registered-applications* (make-hash-table :test #'equal)
  "The registered applications")

;;;
;;; functions
;;;

(defun register-new-application (application)
  (setf (gethash (application-name application) *registered-applications*)
	application)
  (log-info (format nil "Registered ~A application" (application-name application))))

(defun remove-registered-application (application)
  (setf (gethash (application-name application) *registered-applications*) nil))

(defun remove-registered-applications ()
  (setf *registered-applications* (make-hash-table :test #'equal)))

(defun find-registered-application (application-designator &optional (errorp t))
  (typecase application-designator
    (string
     (or (gethash application-designator *registered-applications*)
	 (and errorp (error "Application ~A not found" application-designator))))
    (symbol
     (find-application (string-downcase (string application-designator)) errorp))
    (application
     application-designator)))

(defun registered-applications ()
  (alexandria:hash-table-values *registered-applications*))

(defun map-registered-applications (fn)
  (alexandria:maphash-values fn *registered-applications*))
