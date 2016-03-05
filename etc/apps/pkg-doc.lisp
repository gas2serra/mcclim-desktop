(in-package :cl-desktop)

(register-application "pkg-doc" 'standard-mcclim-application
		      :pretty-name "Pkg Doc"
		      :system-name "clim-pkg-doc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
