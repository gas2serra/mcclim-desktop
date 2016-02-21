(in-package :cl-desktop)

(asdf:require-system "clim-listener")

(register-application "listener" 'standard-mcclim-application
		      :system-name "clim-listener"
		      :debug-p t
		      :entry-fn #'(lambda (&rest args)
				    (clim-listener:run-listener)))


