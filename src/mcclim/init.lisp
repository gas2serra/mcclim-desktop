(in-package :mcclim-desktop)

(defun initialize (&optional (force-p nil))
  (when (or force-p (null *manager*))
    (setf *manager* (make-instance 'mcclim-manager))
    (load (find-init-file))))
