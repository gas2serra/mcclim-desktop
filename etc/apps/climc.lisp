(in-package :cl-desktop-user)

(register-application "climc" 'standard-mcclim-application
		      :pretty-name "Climc"
		      :icon nil
		      :home-page "https://github.com/nlamirault/climc"
		      :git-repo "https://github.com/nlamirault/climc.git"
		      :system-name "climc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
