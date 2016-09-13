(in-package :desktop-internals)

;;;;
;;;; API
;;;;

;;;
;;; Application
;;; 

(defun make-application (name type &rest args)
  (apply #'make-instance type :name (string-downcase name) args))

(defun register-application (name type &rest args)
  (register-new-application
   (apply #'make-application name type args)))

(defun find-application (application-designator &optional (errorp t))
  (or (find-registered-application application-designator nil)
      (discover-application 
       (typecase application-designator
	 (string
	  application-designator)
	 (symbol
	  (string-downcase (string application-designator)))))))

(defun find-applications ()
  (discover-applications))

(defun applications ()
  (registered-applications))

(defun map-applications (fn)
  (map-registered-applications fn))

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

(defun use-application-as-debugger (application-designator)
  (let ((app (find-application application-designator)))
    (unless (application-configured-p app)
      (configure-application app))
    (use-debugger #'(lambda (c e)
		      (funcall (desk:application-entry-fn app) app c e)))))
