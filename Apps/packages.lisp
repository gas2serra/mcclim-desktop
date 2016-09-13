(defpackage :desktop-apps
  (:use :common-lisp)
  (:import-from desktop-launcher
		#:run-launcher)
  (:export
   #:run-launcher))

