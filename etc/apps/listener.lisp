(in-package :cl-desktop-user)

(register-application "listener" 'standard-mcclim-application
		      :pretty-name "Listener"
		      :icon nil
		      :home-page "https://github.com/robert-strandh/McCLIM"
		      :git-repo "https://github.com/robert-strandh/McCLIM.git"
		      :system-name "clim-listener"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
