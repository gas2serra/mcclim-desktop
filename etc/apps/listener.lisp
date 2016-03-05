(in-package :cl-desktop)

(register-application "listener" 'standard-mcclim-application
		      :pretty-name "Listener"
		      :system-name "clim-listener"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
