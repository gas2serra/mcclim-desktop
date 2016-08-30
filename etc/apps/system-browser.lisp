(in-package :cl-desktop-user)

(in-package :cl-desktop-user)

(register-application "system-browser" 'standard-cl-application
		      :pretty-name "System Browser"		      
		      :icon nil
		      :home-page "https://github.com/eudoxia0/trivial-open-browser"
		      :git-repo "https://github.com/eudoxia0/trivial-open-browser.git"
		      :system-name "trivial-open-browser"
		      :debug-p nil
		      :debug-system-p nil)


		    
;;		      :make-command-fn #'(lambda (&rest args)
;;					   (format nil "firefox ~{~A ~}" args)))
;; (trivial-open-browser:open-browser (application-home-page app)))
