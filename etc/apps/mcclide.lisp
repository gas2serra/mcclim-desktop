(in-package :cl-desktop)

(register-application "mcclide" 'standard-mcclim-application
		      :pretty-name "Mcclim IDE"
		      :system-name "mcclide"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
