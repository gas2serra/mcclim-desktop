(in-package :cl-user)

(defpackage mcclim-desktop
  (:use :cl :cl-desktop)
  (:import-from :cl-desktop
		:standard-manager-mixin
		:simple-manager-mixin
		:manager-debugger-hook
		:manager-setup
		:manager
		:*manager*
		:find-init-file
		:launch-application
		:find-application
		:application-config-file
		:application-file)
  (:export
   :launcher-run))

(in-package :mcclim-desktop)

