(in-package :mcclim-desktop)

(setf *debugger-fn* #'clim-debugger:debugger)

(setf *manager* (make-instance 'manager))

(when (asdf:find-system 'sudoku-mcclim nil)
  (add-application "sudoku" 'mcclim-application :system-name "sudoku-mcclim"))

(when (asdf:find-system 'ernestine nil)
  (add-application "ernestine" 'mcclim-application :system-name "ernestine-gui"))

(when (asdf:find-system 'spectacle nil)
  (add-application "spectacle" 'mcclim-application :system-name "spectacle"))

(when (asdf:find-system 'clim-chess nil)
  (add-application "chess" 'mcclim-application :system-name "clim-chess"))

(when (asdf:find-system 'climacs nil)
  (add-application "climacs-rv" 'mcclim-application :system-name "climacs"))

(when (asdf:find-system 'mcclim nil)
  (add-application "listener" 'mcclim-application :system-name "mcclim"))

(when (asdf:find-system 'clim-class-browser nil)
  (add-application "class-browser" 'mcclim-application :system-name "clim-class-browser"))

(when (asdf:find-system 'clim-class-browser nil)
  (add-application "pkg-doc" 'mcclim-application :system-name "clim-pkg-doc"))

(when (asdf:find-system 'climc nil)
  (add-application "climc" 'mcclim-application :system-name "climc"))

(when (asdf:find-system 'climon nil)
  (add-application "climon" 'mcclim-application :system-name "climon"))

(when (asdf:find-system 'gsharp nil)
  (add-application "gsharp" 'mcclim-application :system-name "gsharp"))

(when (asdf:find-system 'beirc nil)
  (add-application "beirc" 'mcclim-application :system-name "beirc"))


