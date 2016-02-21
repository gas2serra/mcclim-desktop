(in-package :cl-desktop)

(ql:quickload "climc")

(register-application "climc" 'standard-mcclim-application
		      :system-name "climc"
		      :entry-fn #'(lambda (&rest args)
				    (climc:start-climc)))


