(in-package :cl-desktop)

(ql:quickload "gsharp")

(register-application "gsharp" 'standard-mcclim-application
		      :system-name "gsharp"
		      :entry-fn #'(lambda (&rest args)
				    (gsharp:gsharp)))
				    


