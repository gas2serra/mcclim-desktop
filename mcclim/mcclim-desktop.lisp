(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :cl-desktop)
  (:import-from :cl-desktop
		:application
		:application-pretty-name
		:standard-manager-mixin
		:simple-manager-mixin
		:manager-debugger-hook
		:manager-debugger-fn
		:manager-setup
		:manager-log-stream
		:reload-application-files
		:manager
		:*manager*
		:launch-application
		:find-application
		:application-config-file
		:application-file
		:configure-application)
  (:export
   :launcher-run))

(in-package :mcclim-desktop)

