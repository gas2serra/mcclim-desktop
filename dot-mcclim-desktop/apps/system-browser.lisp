(in-package :desktop-user)

(register-application "system-browser" 'standard-cl-application
		      :pretty-name "System Browser"		      
		      :icon nil
                      :menu-p nil
		      :home-page "https://github.com/eudoxia0/trivial-open-browser"
		      :git-repo "https://github.com/eudoxia0/trivial-open-browser.git"
		      :system-name "trivial-open-browser")
