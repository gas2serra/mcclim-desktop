(in-package :cl-desktop-user)

(register-application "closure" 'standard-mcclim-application
		      :pretty-name "Closure"
		      :icon nil
		      :home-page "https://github.com/dym/closure"
		      :git-repo "git@github.com:gas2serra/closure.git"
		      :system-name "closure"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
