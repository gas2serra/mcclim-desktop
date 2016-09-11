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
   #:*manager*

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
   
   ;; manager classes
   #:standard-manager
   ;; manager slots
   #:manager-debugger-fn
   #:manager-configured-p
   #:manager-log-stream
   ;; manager protocols
   #:discover-application
   #:discover-applications
   #:add-application
   #:configure-manager
   #:manager-log-info
   #:manager-log-warn
   #:manager-applications
   #:manager-map-applications
   #:load-application-file
   #:reload-application-files
   
   ;; debuggers
   #:*clim-debugger*
   #:*swank-debugger*
   #:debugger-hook
   
   ;; API
   #:make-application
   #:make-manager
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
   #:initialize
   ;; gui
   #:run-launcher
  ))


(defpackage mcclim-desktop-user
  (:use :cl :mcclim-desktop))

(in-package :mcclim-desktop-user)

(initialize)
