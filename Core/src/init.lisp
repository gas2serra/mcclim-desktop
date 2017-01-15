(in-package :desktop-internals)

(defun initialize ()
  (ensure-all-user-directories-exist)
  (find-system-directories)
  (init-debugger)
  (let ((init-file (find-file *init-file-name*)))
    (if init-file
	(load init-file)
	(log-warn (format nil "Init file (~A) not found"
			  *init-file-name*)))))

(defun configure ()
  (let ((config-file (find-file *config-file-name*)))
    (if config-file
	(load config-file)
	(log-warn (format nil "Config file (~A) not found"
			  *config-file-name*)))))
