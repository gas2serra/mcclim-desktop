(in-package :mcclim-desktop)


(setf *applications* (manager-applications *manager*))
#|
(setf *debugger-fn* #'clim-debugger:debugger)

(setf *launcher* (make-instance 'mcclim-launcher))

(when (asdf:find-system 'sudoku-mcclim nil)
  (add-application "sudoku" :system-name "sudoku-mcclim"))

(when (asdf:find-system 'ernestine nil)
  (add-application "ernestine" :system-name "ernestine-gui"))

(when (asdf:find-system 'spectacle nil)
  (add-application "spectacle" :system-name "spectacle"))

(when (asdf:find-system 'spectacle nil)
  (add-application "chess" :system-name "clim-chess"))

(when (asdf:find-system 'climacs nil)
  (add-application "climacs-rv" :system-name "climacs"))

(when (asdf:find-system 'mcclim nil)
  (add-application "listener" :system-name "mcclim"))

(when (asdf:find-system 'clim-class-browser nil)
  (add-application "class-browser" :system-name "clim-class-browser"))

(when (asdf:find-system 'clim-class-browser nil)
  (add-application "pkg-doc" :system-name "clim-pkg-doc"))

(when (asdf:find-system 'climc nil)
  (add-application "climc" :system-name "climc"))

(when (asdf:find-system 'climon nil)
  (add-application "climon" :system-name "climon"))

(when (asdf:find-system 'gsharp nil)
  (add-application "gsharp" :system-name "gsharp"))

(when (asdf:find-system 'gsharp nil)
  (add-application "beirc" :system-name "beirc"))


|#
