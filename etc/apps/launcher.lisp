(in-package :cl-desktop-user)

(register-application "launcher" 'standard-mcclim-application
		      :pretty-name "Launcher"
		      :icon nil
		      :home-page "https://github.com/gas2serra/mcclim-desktop"
		      :git-repo "https://github.com/gas2serra/mcclim-desktop.git"
		      :system-name "mcclim-desktop"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
