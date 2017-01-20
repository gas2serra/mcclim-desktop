(defpackage :desktop-apps
  (:use :common-lisp)
  (:import-from desktop-launcher
		#:run-launcher)
  (:import-from desktop-task-manager
		#:run-task-manager)
  (:import-from desktop-app-manager
		#:run-app-manager)
  (:import-from desktop-apropos
		#:run-apropos-navigator)
  (:import-from desktop-debugger
		#:debugger)
  (:import-from desktop-console
		#:run-console)
  (:export
   #:run-launcher
   #:run-task-manager
   #:run-app-manager
   #:run-apropos-navigator
   #:run-console
   #:debugger))

