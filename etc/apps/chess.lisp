(in-package :cl-desktop)

(ql:quickload "clim-chess")

(register-application "chess" 'standard-mcclim-application
		      :system-name "clim-chess"
		      :entry-fn #'(lambda (&rest args)
				    (clim-chess:chess)))


