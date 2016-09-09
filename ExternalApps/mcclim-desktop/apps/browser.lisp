(in-package :mcclim-desktop-user)

(register-application "browser" 'standard-alias-application
		      :pretty-name "Browser"
		      :icon nil
		      ;;:reference (find-application "closure")
		      :reference (mcclim-desktop-core::find-application-1 mcclim-desktop-core::*manager* "system-browser")
		      )
						   

