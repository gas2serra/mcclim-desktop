(in-package :desktop-user)

(register-application "scliba" 'standard-mcclim-application
		      :pretty-name "Scliba"
		      :icon nil
		      :menu-p t
		      :requires-args-p nil
		      :home-page "https://github.com/admich/scliba"
		      :git-repo "https://github.com/admich/scliba.git"
		      :system-name "scliba-gui")
