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
  (register-launcher-application "climacs-rv" "Climacs RV")
  (register-launcher-application "beirc" "Beirc")
  (register-launcher-application "chess" "Chess")
  (register-launcher-application "climc" "Climc")
  (register-launcher-application "climon" "Climon")
  (register-launcher-application "ernestine" "Ernestine")
  (register-launcher-application "spectacle" "Spectacle")
  (register-launcher-application "listener" "Listener")
  (register-launcher-application "pkg-doc" "PkgDoc")
  (register-launcher-application "gsharp" "Gsharp")
  (register-launcher-application "emacs" "Emacs")
  (register-launcher-application "class-browser" "Class Browser")
  
  )
  

(register-application "launcher" 'standard-mcclim-application
		      :debug-p nil
		      :system-name "mcclim-desktop"
		      :entry-fn #'launcher-entry-fn
		      :config-fn #'launcher-config-fn)


