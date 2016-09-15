(in-package :desktop-user)

(register-application "swank-server" 'standard-cl-application
		      :pretty-name "Swank Server"
		      :icon nil
		      :menu-p t
		      :requires-args-p nil
		      :home-page ""
		      :git-repo ""
		      :system-name "swank")
