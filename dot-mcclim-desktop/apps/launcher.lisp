(in-package :desktop-user)

(register-application "launcher" 'standard-mcclim-application
		      :pretty-name "Launcher"
		      :icon nil
                      :menu-p nil
		      :home-page "https://github.com/gas2serra/mcclim-desktop"
		      :git-repo "https://github.com/gas2serra/mcclim-desktop.git"
		      :system-name "mcclim-desktop-apps")
