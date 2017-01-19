(in-package :desktop-user)

(register-application "app-manager" 'standard-mcclim-application
		      :pretty-name "App Manager"
		      :icon nil
                      :menu-p t
		      :home-page "https://github.com/gas2serra/mcclim-desktop"
		      :git-repo "https://github.com/gas2serra/mcclim-desktop.git"
		      :system-name "mcclim-desktop-apps")
