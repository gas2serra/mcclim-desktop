(in-package :desktop-user)

(register-application "mcclide" 'standard-mcclim-application
		      :pretty-name "Mcclim IDE"
		      :icon nil
		      :home-page "https://github.com/gas2serra/mcclide" 
		      :git-repo "https://github.com/gas2serra/mcclide.git"
		      :system-name "mcclide")
