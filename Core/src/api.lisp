(in-package :mcclim-desktop-core)

;;;;
;;;; API
;;;;

;;;
;;; Application
;;; 

(defun make-application (name type &rest args)
  (apply #'make-instance type :name (string-downcase name) args))

(defun run-app (application &rest args)
  (apply #'run-application (find-application application) args))

(defun launch-app (application &key args cb-fn)
  (funcall #'launch-application (find-application application) :args args :cb-fn cb-fn))

(defun configure-app (application &optional force-p)
  (configure-application (find-application application) force-p))

(defun load-app (application &optional force-p)
  (load-application (find-application application) force-p))

(defun install-app (application &optional force-p)
  (install-application (find-application application) force-p))

;;;
;;; manager
;;;

(defun make-manager (type &rest args)
  (setf *manager* (apply #'make-instance type args)))
#|
(defun log-info (msg &optional (manager *manager*))
  (manager-log-info manager msg))

(defun log-warn (msg &optional (manager *manager*))
  (manager-log-warn manager msg))
|#
(defun register-application (name type &rest args)
  (add-application *manager*
		   (apply #'make-application name type args)))

(defun find-application (application-designator &optional (errorp t) (manager *manager*))
  (typecase application-designator
    (string
     (discover-application *manager* (string-downcase application-designator)))
    (symbol
     (find-application (string-downcase (string application-designator)) errorp manager))
    (application
     application-designator)))

(defun applications ()
  (manager-applications *manager*))

(defun map-applications (fn)
  (manager-map-applications *manager* fn))
