(in-package :mcclim-desktop)

(register-application "editor" 'standard-alias-application :reference (find-application "climacs"))


#|
(add-application "editor" 'proxy-application
		 :reference (find-application *manager* "climacs")
		 :entry-fn #'(lambda (application &rest args)
			       (apply 'run-application
				      (application-link-reference application)
				      args)))
|#
