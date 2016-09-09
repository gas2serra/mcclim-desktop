(in-package :mcclim-desktop-debugger)

;;; LOAD THE CLIM DEBUGGER
(load (merge-pathnames "Apps/Debugger/clim-debugger.lisp"
		       (asdf:component-pathname (asdf:find-system "mcclim"))))		       


