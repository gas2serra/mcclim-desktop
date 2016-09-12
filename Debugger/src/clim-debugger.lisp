(in-package :desktop-internals)

;;; load the clim debugger
(load (merge-pathnames "Apps/Debugger/clim-debugger.lisp"
		       (asdf:component-pathname (asdf:find-system "mcclim"))))
