(in-package :cl-desktop-user)

(register-application "class-browser" 'standard-mcclim-application
		      :pretty-name "Class Browser"
		      :icon nil
		      :home-page "https://github.com/pocket7878/clim-class-browser"
		      :git-repo "https://github.com/pocket7878/clim-class-browser.git"
		      :system-name "clim-class-browser"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
