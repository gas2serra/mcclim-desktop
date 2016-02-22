(in-package :mcclim-desktop)

(defun launcher-run ()
  (init)
;  (let* ((manager-application (make-instance 'standard-cl-application				       
;					      :name "manager"
;					      :system-name "mcclim-desktop"
;					      :config-file (cl-desktop::default-config-file "manager")
;					      :debug-p nil
;					      :entry-fn #'(lambda (&rest args) t))))
;        (run-application manager-application)
  (launch-application (find-application "launcher")))
