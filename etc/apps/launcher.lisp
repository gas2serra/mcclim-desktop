(in-package :cl-desktop)

(register-application "launcher" 'standard-mcclim-application
		      :pretty-name "Launcher"
		      :system-name "mcclim-desktop"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
