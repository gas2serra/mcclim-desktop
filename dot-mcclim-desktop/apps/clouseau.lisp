(in-package :desktop-user)

(register-application "clouseau" 'standard-mcclim-application
		      :pretty-name "Clouseau"
		      :icon nil
		      :menu-p nil
		      :home-page "https://github.com/robert-strandh/McCLIM"
		      :git-repo "https://github.com/robert-strandh/McCLIM.git"
		      :system-name "clouseau")
