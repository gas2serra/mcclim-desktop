(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'(lambda (application &rest args)
                                               (sudoku-mcclim:run)))
      
