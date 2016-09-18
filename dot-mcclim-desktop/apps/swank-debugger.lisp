(in-package :desktop-user)

(register-application "swank-debugger" 'standard-debugger-application
		      :pretty-name "Swank Debugger"
		      :icon nil
		      :menu-p nil
		      :requires-args-p t
		      :home-page ""
		      :git-repo ""
		      :system-name "swank")
