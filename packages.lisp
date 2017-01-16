(in-package :common-lisp-user)

(defpackage :desktop
  (:use :common-lisp)
  (:nicknames :desk)
  (:export
   ;; initialization
   #:initialize
   #:configure
   
   ;; debugger
   #:install-debugger-globally
   #:use-debugger
   #:use-external-debugger
 
   ;; logger
   #:use-logger
   #:make-logger
   #:standard-logger
   #:log-info
   #:log-warn
   #:log-error

   ;; global application variables
   #:*application*
   #:*application-style*

   ;; application classes
   #:standard-cl-application
   #:standard-mcclim-application
   #:standard-shell-application
   #:standard-alias-application
   #:standard-debugger-application
   #:standard-mcclim-debugger-application
   
   ;; application slots
   #:application-name
   #:application-pretty-name
   #:application-icon
   #:application-style
   #:application-menu-p
   #:application-requires-args-p
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
   #:application-debugger-fn
   ;; application protocols
   #:run-application
   #:launch-application
   #:configure-application
   #:load-application
   #:install-application
   #:application-file
   #:application-config-file
   #:application-style-file
   
   ;; refresh
   #:refresh-application
   #:refresh-applications

   ;; standard pathname
   
   ;; API
   #:make-application
   #:register-application
   #:find-application
   #:find-applications
   #:applications
   #:map-applications
   #:run-app
   #:launch-app
   #:configure-app
   #:load-app
   #:install-app
   #:use-application-as-debugger
   #:use-application-as-external-debugger
   ))

(defpackage :desktop-extensions
  (:use :desktop :common-lisp)
  (:export
   ;; debugger
   #:*debugger*
   #:*external-debugger*
   
   ;; logger
   #:*logger*
   #:with-logger
   #:logger-log-info
   #:logger-log-warn
   #:logger-log-error
   #:logger
   #:stream-logger-mixin
   #:logger-stream
   
   ;; application mixin
   #:simple-application-mixin
   #:simple-cl-application-mixin
   #:simple-shell-application-mixin
   #:standard-application-mixin
   #:standard-cl-application-mixin
   #:simple-debugger-application-mixin

   ;; application
   #:application
   #:cl-application
   #:mcclim-application
   #:link-application
   #:alias-application
   #:proxy-application 
   #:shell-application
   #:need-reconfigure-application
   #:load-application-config-file
   #:load-application-style-file
   ;; applications
   #:*registered-applications*
   #:register-new-application
   #:remove-registered-application
   #:remove-registered-applications
   #:find-registered-application
   #:registered-applications
   #:map-registered-applications
   ;; standard pathname
   #:find-user-file
   #:create-user-file
   #:find-user-files
   #:find-system-file
   #:find-system-files
   #:find-system-directories
   #:find-file   
   ;; discovering
   #:discover-application
   #:discover-applications
   ))

(defpackage :desktop-sys
  (:use :common-lisp)
  (:export
   #:copy-to-x11-clipboard
   #:paste-from-x11-clipboard
   #:copy-to-kill-ring
   #:paste-from-kill-ring
   #:take-x11-screenshot
   ))

(defpackage :desktop-internals
  (:use :desktop :desktop-extensions :desktop-sys :common-lisp)
  (:nicknames :deski))

(defpackage :desktop-user
  (:use :desktop :common-lisp))
