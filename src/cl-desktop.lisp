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
   ))

(in-package :cl-desktop)

