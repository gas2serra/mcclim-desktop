(in-package :cl-desktop)

(register-application "climc" 'standard-mcclim-application
		      :pretty-name "Climc"
		      :system-name "climc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
