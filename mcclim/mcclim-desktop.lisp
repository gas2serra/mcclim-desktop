(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :cl-desktop)
  (:import-from :cl-desktop
		;; application classes
		:application
		;; manager
		:manager-log-stream
		:log-warn
		:initialize-manager
		;; standard pathnames
		:find-file)
  (:export
   :launcher-run))

(in-package :mcclim-desktop)

