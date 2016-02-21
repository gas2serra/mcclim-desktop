(in-package :cl-desktop)

(ql:quickload "beirc")

(register-application "beirc" 'standard-mcclim-application
		      :system-name "beirc"
		      :entry-fn #'(lambda (&rest args)
				    (beirc:beirc)))


