(in-package :cl-user)

(defpackage mcclim-desktop-core
  (:use :cl)
  (:export
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
   #:application-default-style-file
   
   ;; manager classes
   #:standard-manager
   ;; manager slots
   #:manager-debugger-fn
   #:manager-configured-p
   #:manager-log-stream
   ;; manager protocols
   #:get-application
   #:add-application
   #:configure-manager
   #:manager-log-info
   #:manager-log-warn
   #:manager-applications
   #:manager-map-applications
   #:discover-application
   #:discover-applications
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
   ))
