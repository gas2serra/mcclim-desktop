(in-package :mcclim-desktop)

;;(add-application "editor" 'alias-application :to (find-application *manager* "climacs"))

(add-application "editor" 'link-application
		 :to (find-application *manager* "climacs")
		 :entry-fn #'(lambda (application &rest args)
			       (apply 'run-application
				      (application-link-to application)
				      args)))

