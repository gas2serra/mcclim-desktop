(in-package :cl-desktop)

(register-application "chess" 'standard-mcclim-application
		      :pretty-name "Chess"
		      :system-name "clim-chess"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
