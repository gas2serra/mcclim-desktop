(in-package :mcclim-desktop-core)

#|
(defun initialize (&optional force-p)
  (when (or force-p (null *manager*))
    (refresh-desktop-search-pathnames)
    (let ((init-file (find-file *init-file-name*)))
      (when init-file
	(load init-file))))
   (configure-manager *manager*))
|#

(defun init ()
  (uiop:ensure-all-directories-exist
   (list *user-directory*
	 (uiop:merge-pathnames*
	  *application-file-directory*
	  *user-directory*)
	 (uiop:merge-pathnames*
	  *application-config-directory*
	  *user-directory*)))
  (refresh-desktop-search-pathnames)
  (let ((init-file (find-file *init-file-name*)))
    (if init-file
	(load init-file)
	(log-warn (format nil "Init file (~A) not found"
			  *manager-config-file-name*))))
  (configure))
  

(defun configure ()
  (let ((config-file (find-file *manager-config-file-name*)))
    (if config-file
	(load config-file)
	(log-warn (format nil "Config file (~A) not found"
			  *manager-config-file-name*)))))
