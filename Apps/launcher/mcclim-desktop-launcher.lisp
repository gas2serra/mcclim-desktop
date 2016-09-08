(in-package :cl-user)

(defpackage mcclim-desktop-launcher
  (:use :cl)
  (:import-from :cl-desktop
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
		*manager*)
  (:export
   :run-launcher))

(in-package :mcclim-desktop-launcher)
