(in-package :desktop-user)

(register-application "browser" 'standard-alias-application
		      :pretty-name "Browser"
		      :icon nil
		      ;;:reference (find-application "closure")
		      :reference (find-application "system-browser")
		      )
						   

