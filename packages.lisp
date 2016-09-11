(in-package :common-lisp-user)

(defpackage :desktop
  (:use :common-lisp)
  (:nicknames :desk)
  (:export
      ;; debugger
   #:*debugger*
   #:debugger-hook
   #:*clim-debugger*
   #:*swank-debugger*

   ;; logger
   #:*logger*
   #:logger-stream
   #:make-logger
   #:standard-logger

      ;; global
   #:*application*
   #:*application-style*

   ;; application classes
   #:application
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

   #:find-file

   ;; init
   #:init
   ))

(defpackage :desktop-sys
  (:use :common-lisp)
  (:export
   ))

(defpackage :desktop-internals
  (:use :desktop :desktop-sys :common-lisp)
  (:nicknames :deski))

(defpackage :desktop-user
  (:use :desktop :common-lisp))
