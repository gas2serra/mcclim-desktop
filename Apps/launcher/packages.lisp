(in-package :cl-user)

(defpackage desktop-launcher
  (:use :desktop :desktop-extensions :cl)
  (:export
   :run-launcher))

(in-package :desktop-launcher)
