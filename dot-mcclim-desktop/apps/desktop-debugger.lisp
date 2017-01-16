(in-package :desktop-user)

(register-application "desktop-debugger" 'standard-mcclim-debugger-application
		      :pretty-name "Desktop Debugger"
		      :icon nil
		      :menu-p nil
		      :requires-args-p t
		      :home-page "https://github.com/gas2serra/mcclim-desktop/tree/master/Apps/debugger/"
		      :git-repo "https://github.com/gas2serra/mcclim-desktop.git"
		      :system-name "mcclim-desktop-apps")
