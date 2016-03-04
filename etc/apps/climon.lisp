(in-package :cl-desktop)

(ql:quickload "climon")

(register-application "climon" 'standard-mcclim-application
		      :system-name "climon")
