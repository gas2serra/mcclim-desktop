(in-package :cl-user)

(defpackage mcclim-desktop-launcher
  (:use :cl)
  (:import-from :mcclim-desktop-core
		;; application classes
		:application
		:find-application
		:launch-application
		;; manager
		:refresh-application
		:manager-log-stream
		:log-warn
		:initialize-manager
		;; standard pathnames
		:find-file

		#:application-file
		#:configure-application
		#:application-config-file
		#:application-home-page
		#:*manager*)
  (:export
   :run-launcher))

(in-package :mcclim-desktop-launcher)

