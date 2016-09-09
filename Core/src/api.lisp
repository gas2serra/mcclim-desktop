(in-package :mcclim-desktop-core)

;;;;
;;;; API
;;;;


;;;
;;; Application
;;; 

(defun make-app (name type &rest args)
  (apply #'make-instance type :name name args))

(defun app (app-designator)
  (typecase application
    (string
     (find-app application))
    (symbol
     (string app-designator))
    (application
     application)))

(defun run-app (application &rest args)
  (apply #'run-application (app application) args))

(defun launch-app (application &rest args)
  (apply #'launch-application (app application) args))

(defun configure-app (application &optional force-p)
  (configure-application (app application) force-p))

(defun load-app (application &optional force-p)
  (load-application (app application) force-p))

(defun install-app (application &optional force-p)
  (install-application (app application) force-p))

;;;
;;; manager
;;;

(defun get-app (name &optional (errorp t) (manager *manager*))
  (get-application-1 manager name errorp))

(defun find-app (name &optional (errorp t) (manager *manager*))
  (find-application-1 manager name errorp))

(defun register-application (name type &rest args)
  (add-application-1 *manager*
		     (apply #'make-app name type args)))

(defun configure-man (&optional force-p (manager *manager*))
  (configure-manager managet force-p))

(defun log-info (msg &optional (manager *manager*))
  (manager-log-info manager msg))

(defun log-warn (msg &optional (manager *manager*))
  (manager-log-warn manager msg))
