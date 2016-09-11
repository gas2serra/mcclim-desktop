(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :mcclim-desktop-core :mcclim-desktop-debugger)
  (:nicknames :desktop)
  (:import-from :mcclim-desktop-launcher
		:run-launcher)
  (:export
   ;; logger
   #:*logger*
   #:make-logger
   #:logger-stream
   #:standard-logger

   ;; debugger
   #:*debugger*
   #:debugger-hook

   ;; global
   #:*application*
   #:*application-style*

   ;; application classes
   #:standard-cl-application
   #:standard-mcclim-application
   #:standard-shell-application
   #:standard-alias-application
   ;; application slots
   #:application-name
   #:application-pretty-name
   #:application-icon
   #:application-style
   #:application-configured-p
   #:application-home-page
   #:application-git-repo
   #:application-system-name
   #:application-debug-system-p
   #:application-loaded-p
   #:application-installed-p
   #:application-frame-class
   #:application-link-reference
   #:application-entry-fn
   #:application-make-command-fn
   ;; application protocols
   #:run-application
   #:launch-application
   #:configure-application
   #:load-application
   #:install-application
   #:application-file
   #:application-config-file
   #:application-style-file
   
   ;; debuggers
   #:*clim-debugger*
   #:*swank-debugger*
   #:debugger-hook


   #:discover-application
   #:discover-applications

   
   ;; API
   #:make-application
   #:register-application
   #:find-application
   #:log-info
   #:log-warn
   #:applications
   #:map-applications
   #:run-app
   #:launch-app
   #:configure-app
   #:load-app
   #:install-app
   ;; init
   #:init
   ;; gui
   #:run-launcher
  ))


(defpackage mcclim-desktop-user
  (:use :cl :mcclim-desktop))

(in-package :mcclim-desktop-user)

(init)
