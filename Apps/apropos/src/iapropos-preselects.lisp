(in-package :desktop-apropos)

;;;
;;; Preselect
;;;

(defun command-internal-preselect (iapropos)
  (let ((scanner (cl-ppcre:create-scanner "^COM-" :case-insensitive-mode t)))
    (flet ((filter-fn (symbol)
	   (let* ((name (symbol-name symbol)))
	     (and (cl-ppcre::scan scanner name)
		  (cl-ppcre::scan #\% name)))))
      (setf (iapropos-filter-fn iapropos) #'filter-fn)
      (setf (iapropos-subclass-of iapropos) nil)
      (setf (iapropos-metaclass-of iapropos) nil)
      (setf (iapropos-bound-to iapropos) :function))))

(defun command-table-preselect (iapropos)
  (flet ((filter-fn (symbol)
	   (clim:find-command-table symbol :errorp nil)))
    (setf (iapropos-filter-fn iapropos) #'filter-fn)
    (setf (iapropos-subclass-of iapropos) nil)
    (setf (iapropos-metaclass-of iapropos) nil)  
    (setf (iapropos-bound-to iapropos) nil)))

(defun presentation-type-preselect (iapropos)
    (setf (iapropos-filter-fn iapropos) nil)
    (setf (iapropos-subclass-of iapropos) nil)
    (setf (iapropos-metaclass-of iapropos) 'climi::presentation-type-class)
    (setf (iapropos-bound-to iapropos) :class))

(defun view-type-preselect (iapropos)
    (setf (iapropos-filter-fn iapropos) nil)
    (setf (iapropos-subclass-of iapropos) 'clim:view)
    (setf (iapropos-metaclass-of iapropos) nil)
    (setf (iapropos-bound-to iapropos) :class))
