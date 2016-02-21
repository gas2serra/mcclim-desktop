(in-package :cl-desktop)

(ql:quickload "ernestine-gui")

(register-application "ernestine" 'standard-mcclim-application
		      :system-name "ernestine-gui"
		      :entry-fn #'(lambda (&rest args)
				    (ernestine-gui:player)))


