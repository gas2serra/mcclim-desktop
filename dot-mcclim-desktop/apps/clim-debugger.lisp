(in-package :desktop-user)

(register-application "clim-debugger" 'standard-mcclim-debugger-application
		      :pretty-name "Clim Debugger"
		      :icon nil
		      :menu-p nil
		      :requires-args-p t
		      :home-page ""
		      :git-repo ""
		      :system-name "mcclim")
