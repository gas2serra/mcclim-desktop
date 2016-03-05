(in-package :cl-desktop)

(register-application "clim-demo" 'standard-mcclim-application
		      :pretty-name "Clim Demo"
		      :system-name "clim-examples"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
