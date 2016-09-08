(in-package :mcclim-desktop-user)

(register-application "chess" 'standard-mcclim-application
		      :pretty-name "Chess"
		      :icon nil
		      :home-page "https://github.com/stassats/clim-chess"
		      :git-repo "https://github.com/stassats/clim-chess.git"
		      :system-name "clim-chess"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
