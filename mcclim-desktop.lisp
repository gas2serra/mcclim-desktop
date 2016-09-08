(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :cl-desktop)
  (:import-from :mcclim-desktop-launcher
		:run-launcher)
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
   :application-home-page

   :initialize-manager
   
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

   #:run-launcher
  ))


(defpackage cl-desktop-user
  (:use :cl :cl-desktop))

(in-package :cl-desktop)

(mcclim-desktop:initialize-manager)
