(in-package :mcclim-desktop)

(ql:quickload "mcclim-desktop")

(defun launcher-entry-fn (application &rest args)
  (declare (ignore args))
  (mcclim-desktop::run-launcher-gui))


(defun launcher-config-fn (application)
  (setf *applications* nil)
  (register-launcher-application "sudoku" "Sudoku")
  (register-launcher-application "climacs" "Climacs")
  (register-launcher-application "editor" "Editor")
  (register-launcher-application "beirc" "Beirc")
  (register-launcher-application "chess" "Chess")
  (register-launcher-application "climc" "Climc")
  (register-launcher-application "climon" "Climon")
  (register-launcher-application "ernestine" "Ernestine")
  (register-launcher-application "spectacle" "Spectacle")
  (register-launcher-application "listener" "Listener")
  (register-launcher-application "pkg-doc" "PkgDoc")
  (register-launcher-application "scigraph" "Scigraph")
  (register-launcher-application "gsharp" "Gsharp")
  (register-launcher-application "emacs" "Emacs")
  (register-launcher-application "mcclide" "McCLIM IDE")
  (register-launcher-application "class-browser" "Class Browser")
  (register-launcher-application "clim-demo" "Clim Demo"))
  
(setf (application-entry-fn *application*) #'launcher-entry-fn)
(setf (application-config-fn *application*) #'launcher-config-fn)


