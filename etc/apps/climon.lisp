(in-package :cl-desktop)

(ql:quickload "climon")

(register-application "climon" 'standard-mcclim-application
		      :system-name "climon"
		      :entry-fn #'(lambda (&rest args)
				    (climon:climon)))


