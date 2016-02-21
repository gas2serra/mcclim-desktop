(in-package :cl-desktop)

(asdf:require-system "sudoku-mcclim")

(defun sudoku-entry-fn (application &rest args)
  (declare (ignore args))
  (sudoku-mcclim:run))
 
(register-application "sudoku" 'standard-mcclim-application
		      :system-name "sudoku-mcclim"
		      :debug-system-p nil
		      :entry-fn #'sudoku-entry-fn)
