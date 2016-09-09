(in-package :mcclim-desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (application &rest args)
	  (declare (ignore application args))
	  (mcclim-desktop-launcher::run-launcher-gui)))

(setf mcclim-desktop-launcher::*applications* nil)

(mcclim-desktop-launcher::register-launcher-applications
 "sudoku"
 "climacs"
 "editor"
 "beirc"
 "chess"
 "climc"
 "climon"
 "ernestine"
 "spectacle"
 "listener"
 "pkg-doc" 
 "scigraph"
 "gsharp" 
 "emacs" 
 "mcclide"
 "class-browser"
 "clim-demo"
 "closure"
 "apropos-navigator"
 "task-manager"
 "dired"
 "browser")
