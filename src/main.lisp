(in-package :mcclim-desktop)

(defun run ()
  (let* ((manager-application (make-instance 'standard-cl-application
					      :name "manager"
					      :system-name "mcclim-desktop"
					      :entry-fn #'(lambda () t)))
	 (launcher-application (make-instance 'standard-mcclim-application
					      :name "launcher"
					      :system-name "mcclim-desktop"
					      :entry-fn #'run-launcher-gui)))
    (run-application manager-application)
    (run-application launcher-application)))
