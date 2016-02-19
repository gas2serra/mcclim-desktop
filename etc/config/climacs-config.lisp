(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'(lambda (application &rest args)
					       (if (null args)
						   (climacs:climacs)
						   (climacs:edit-file (car args)))))
