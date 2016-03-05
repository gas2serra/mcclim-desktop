(in-package :cl-desktop)

(register-application "climacs" 'standard-mcclim-application
		      :pretty-name "Climacs"
		      :system-name "climacs"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
