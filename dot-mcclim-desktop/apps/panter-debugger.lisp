(in-package :desktop-user)

(register-application "panter-debugger" 'standard-mcclim-application
		      :pretty-name "Panter Debugger"
		      :icon nil
		      :menu-p nil
		      :home-page "https://github.com/gas2serra/mcclim-panter/tree/master/Apps/debugger/"
		      :git-repo "https://github.com/gas2serra/mcclim-panter.git"
		      :system-name "mcclim-panter")
