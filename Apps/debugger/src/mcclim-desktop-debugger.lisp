(in-package :cl-user)

(defpackage desktop-debugger 
  (:use :cl :desktop :desktop-extensions)
  (:import-from :clim
		)
  (:export
   :debugger
   ))

(in-package :desktop-debugger)
