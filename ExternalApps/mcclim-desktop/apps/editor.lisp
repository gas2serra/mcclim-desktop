(in-package :mcclim-desktop)

(register-application "editor" 'standard-alias-application
		      :pretty-name "Editor"
		      :icon nil
		      :reference (mcclim-desktop-core::find-application-1 *manager* "climacs")
		      ;;:reference (find-application "emacs")
		      )
						   

