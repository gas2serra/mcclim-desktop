(in-package :cl-desktop)

(ql:quickload "spectacle")

(register-application "spectacle" 'standard-mcclim-application
		      :system-name "spectacle"
		      :entry-fn #'(lambda (&rest args)
				    (spectacle:spectacle)))


