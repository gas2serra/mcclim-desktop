(in-package :desktop-user)

(register-application "editor" 'standard-alias-application
		      :pretty-name "Editor"
		      :icon nil
		      :reference (find-application "climacs")
		      ;;:reference (find-application "emacs")
		      )
						   

