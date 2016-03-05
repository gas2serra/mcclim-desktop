(in-package :mcclim-desktop)

(register-application "editor" 'standard-alias-application
		      :pretty-name "Editor"
		      :icon nil
		      :reference (find-application "climacs"))
