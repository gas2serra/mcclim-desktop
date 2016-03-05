(in-package :cl-desktop)

(register-application "sudoku" 'standard-mcclim-application
		      :pretty-name "Sudoku"
		      :system-name "sudoku-mcclim"
		      :debug-p nil
		      :debug-system-p nil
		      :frame-class nil)
