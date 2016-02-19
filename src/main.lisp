(in-package :mcclim-desktop)

(defun run ()
  (let* ((manager-application (make-instance 'standard-cl-application
					      :name "manager"
					      :system-name "mcclim-desktop"
					      :debug-p nil
					      :entry-fn #'(lambda (&rest args) t)))
	 (launcher-application (make-instance 'standard-mcclim-application
					      :name "launcher"
					      :debug-p nil
					      :system-name "mcclim-desktop"
					      :entry-fn #'run-launcher-gui)))
    (run-application manager-application)
    (run-application launcher-application)))
