(in-package :cl-user)

(defpackage cl-desktop
  (:use :cl)
  (:export
   :*application*
   :register-application
   :STANDARD-MCCLIM-APPLICATION
   :STANDARD-SHELL-APPLICATION
   :STANDARD-ALIAS-APPLICATION
   :application-pretty-name
   :application-entry-fn
   :application-config-fn
   :find-application
   ))

(defpackage cl-desktop-user
  (:use :cl :cl-desktop))

(in-package :cl-desktop)

