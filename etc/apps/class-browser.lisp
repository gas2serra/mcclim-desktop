(in-package :cl-desktop)

(register-application "class-browser" 'standard-mcclim-application
		      :pretty-name "Class Browser"
		      :system-name "clim-class-browser"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
