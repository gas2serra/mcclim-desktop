(in-package :cl-user)

(defpackage mcclim-desktop-launcher
  (:use :cl)
  (:import-from :mcclim-desktop-core
		;; application classes
		:application
		:find-application
		:launch-application
		;; manager
		:load-application-file
		:log-warn
		:initialize
		;; standard pathnames
		:find-file

		#:application-file
		#:configure-application
		#:application-config-file
		#:application-home-page
		#:*manager*
		;; logger
		#:*logger*
		#:logger-stream
		)
  (:export
   :run-launcher))

(in-package :mcclim-desktop-launcher)

