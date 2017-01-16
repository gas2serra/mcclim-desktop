(defpackage :desktop-apps
  (:use :common-lisp)
  (:import-from desktop-launcher
		#:run-launcher)
  (:import-from desktop-task-manager
		#:run-task-manager)
  (:import-from desktop-apropos
		#:run-apropos-navigator)
  (:import-from desktop-debugger
		#:debugger)

  (:export
   #:run-launcher
   #:run-task-manager
   #:run-apropos-navigator
   #:debugger))

