(in-package :cl-user)
(defpackage mcclim-desktop
  (:use :cl :cl-desktop)
  (:import-from :cl-desktop
		:application

		:run-application
		:launch-application
		:configure-application
		:application-file
		:application-config-file
		
		
		:mcclim-application
		:link-application
		:alias-application
		:proxy-application
		
		:nocl-application
		:shell-application
		
		:standard-cl-application
		:standard-mcclim-application
		:standard-alias-application 
		:standard-link-application
		:standard-shell-application

		:standard-application-mixin

		
		:application-name
		:application-pretty-name
		:application-configured-p
		:application-configuration-time
		:application-entry-fn
		
		:*application*
		:*debugger-fn*
		
		:cl-application
		:load-application
		:application-debug-p
		:application-system-name
		:application-loaded-p
		:application-loading-time
		
		:pretty-name
		:name
		:configuration-time
		:configured-p
		:debug-p

		:default-system-application-file
		:default-user-application-file
		:default-application-file
		:default-user-config-file
		:default-system-config-file
		:default-config-file
		:load-default-application-file

		:*manager*
		:manager-applications
		:find-application
		:register-application

		standard-manager-mixin
		simple-manager-mixin
		manager
		)
  (:export
   :run
   ))

(in-package :mcclim-desktop)


