(in-package :cl-desktop-user)

(register-application "beirc" 'standard-mcclim-application
		      :pretty-name "Be IRC"
		      :icon nil
		      :home-page "https://github.com/MrNeutron/beirc"
		      :git-repo "https://github.com/MrNeutron/beirc.git"
		      :system-name "beirc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
