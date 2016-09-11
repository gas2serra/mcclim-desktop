(in-package :cl-user)

(defpackage mcclim-desktop-core
  (:use :cl)
  (:export
   ;; debugger
   #:*debugger*
   #:debugger-hook

   ;; logger
   #:*logger*
   #:logger-stream
   #:make-logger
   #:standard-logger

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


   #:discover-application
   #:discover-applications
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
   #:init
   ))
