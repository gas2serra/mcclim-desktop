(in-package :cl-desktop)

(register-application "beirc" 'standard-mcclim-application
		      :pretty-name "Be IRC"
		      :system-name "beirc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
