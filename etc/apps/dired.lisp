(in-package :cl-desktop-user)

(register-application "dired" 'standard-mcclim-application
		      :pretty-name "Dired"
		      :icon nil
		      :home-page "https://github.com/gas2serra/flexi-trivial-dired"
		      :git-repo "https://github.com/gas2serra/flexi-trivial-dired.git"
		      :system-name "ftd"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
