(in-package :mcclim-desktop)

(defparameter *manager-mcclim-config-file-name* "manager-mcclim-config.lisp")

(defun configure-mcclim-manager (manager &optional force-p)
  (declare (ignore force-p))
  (let ((config-file (find-file *manager-mcclim-config-file-name*)))
    (when config-file
	(if (probe-file config-file)
	    (let ((*manager* manager))
	      (load config-file))
	    (log-warn (format nil "Config file (~A) for manager not found" config-file))))))


(defun initialize-mcclim-manager (&optional (force-p nil))
  (initialize-manager)
  (configure-mcclim-manager *manager*))
