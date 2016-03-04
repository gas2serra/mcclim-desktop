(in-package :cl-desktop)

(asdf:require-system "sudoku-mcclim")

(defun sudoku-entry-fn (application &rest args)
  (declare (ignore args))
  (sudoku-mcclim:run))
 
(setf (application-entry-fn *application*) #'sudoku-entry-fn)



