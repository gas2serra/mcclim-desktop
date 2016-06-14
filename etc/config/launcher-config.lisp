(in-package :cl-desktop-user)

(setf *application* (find-application "launcher"))

(defun launcher-entry-fn (application &rest args)
  (declare (ignore application args))
  (mcclim-desktop::run-launcher-gui))

(defun launcher-config-fn (application)
  (declare (ignore application))
  (setf mcclim-desktop::*applications* nil)
  (mcclim-desktop::register-launcher-applications "sudoku"
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
						  "closure"))

(setf (application-entry-fn *application*) #'launcher-entry-fn)
(setf (application-config-fn *application*) #'launcher-config-fn)
