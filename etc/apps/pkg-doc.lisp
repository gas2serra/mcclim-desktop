(in-package :cl-desktop-user)

(register-application "pkg-doc" 'standard-mcclim-application
		      :pretty-name "Pkg Doc"
		      :icon nil
		      :home-page "https://github.com/jschatzer/clim-pkg-doc"
		      :git-repo "https://github.com/jschatzer/clim-pkg-doc.git"
		      :system-name "clim-pkg-doc"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
