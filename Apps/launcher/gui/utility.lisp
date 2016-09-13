(in-package :desktop-launcher)

(defun edit-file (filename &key cb-fn)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename) :cb-fn cb-fn)))

#|
(defun register-launcher-applications (&rest names)
  (dolist (name names)
    (when (find-application name) 
      (push name *applications*))))
|#
