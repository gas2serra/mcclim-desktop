(in-package :cl-user)

(defpackage mcclim-desktop-launcher
  (:use :desktop-extensions :cl)
  (:import-from :desktop
		;; application classes
		#:application-pretty-name
		#:find-application
		#:launch-application
		;; manager
		#:log-warn
		;; standard pathnames
		;;#:find-file
		#:application-file
		#:configure-application
		#:application-config-file
		#:application-home-page
		#:applications
		;; logger
		)
  ;;(:import-from :desktop-internals
  ;;#:application
  ;;		)

  (:export
   :run-launcher))

(in-package :mcclim-desktop-launcher)

