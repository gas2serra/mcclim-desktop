(in-package :mcclim-desktop-core)

(defun initialize-manager (&optional (force-p nil))
  (when (or force-p (null *manager*))
    (let ((init-file (find-file *init-file-name*)))
      (when init-file
	(load init-file))))
  (configure-manager *manager*))
