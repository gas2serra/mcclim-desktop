(in-package :cl-user)

(defpackage cl-desktop
  (:use :cl)
  (:export
   ;; global
   :*application*
   :*manager*
   ;; application classes
   :standard-cl-application
   :standard-mcclim-application
   :standard-shell-application
   :standard-alias-application
   ;; application
   :application-pretty-name
   :application-entry-fn
   :application-config-fn
   
   :launch-application
   :configure-application
   :load-application
   
   :application-config-file
   :application-file
   ;; manager utilities
   :register-application
   :find-application
   ;; manager
   :manager-force-debug-p
   :manager-debugger-fn
   ))

(defpackage cl-desktop-user
  (:use :cl :cl-desktop))

(in-package :cl-desktop)

