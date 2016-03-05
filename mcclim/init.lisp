(in-package :mcclim-desktop)

(defun initialize (&optional (force-p nil))
  (if (or force-p (null *manager*))
      (setf *manager* (make-instance 'mcclim-manager))
      (refresh-applications *manager*)))

      
      
  
