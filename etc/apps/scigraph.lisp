(in-package :cl-desktop-user)

(register-application "scigraph" 'standard-mcclim-application
		      :pretty-name "Sci Graph"
		      :icon nil
		      :home-page "https://github.com/robert-strandh/McCLIM"
		      :git-repo "https://github.com/robert-strandh/McCLIM.git"
		      :system-name "scigraph"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
