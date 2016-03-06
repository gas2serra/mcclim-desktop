(in-package :cl-desktop-user)

(register-application "sudoku" 'standard-mcclim-application
		      :pretty-name "Sudoku"
		      :icon nil
		      :home-page "https://github.com/tortkis/sudoku-mcclim"
		      :git-repo "https://github.com/tortkis/sudoku-mcclim.git"
		      :system-name "sudoku-mcclim"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
