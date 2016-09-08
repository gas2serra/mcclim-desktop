(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :mcclim-desktop-core)
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


   #:*clim-debugger*
   #:*swank-debugger*
  ))


(defpackage mcclim-desktop-user
  (:use :cl :mcclim-desktop))

(in-package :mcclim-desktop)

(mcclim-desktop:initialize-manager)
