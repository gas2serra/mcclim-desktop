(in-package :desktop-user)

(register-application "console" 'standard-mcclim-application
		      :pretty-name "Console"
		      :icon nil
                      :menu-p t
		      :home-page "https://github.com/gas2serra/mcclim-desktop"
		      :git-repo "https://github.com/gas2serra/mcclim-desktop.git"
		      :system-name "mcclim-desktop-console")
