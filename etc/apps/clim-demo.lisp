(in-package :cl-desktop-user)

(register-application "clim-demo" 'standard-mcclim-application
		      :pretty-name "Clim Demo"
		      :icon nil
		      :home-page "https://github.com/robert-strandh/McCLIM"
		      :git-repo "https://github.com/robert-strandh/McCLIM.git"
		      :system-name "clim-examples"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
