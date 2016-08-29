(in-package :cl-desktop-user)

(register-application "panter" 'standard-mcclim-application
		      :pretty-name "Panter (debugging utilities)"
		      :icon nil
		      :home-page "https://github.com/gas2serra/mcclim-panter"
		      :git-repo "https://github.com/gas2serra/mcclim-panter.git"
		      :system-name "mcclim-panter"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
