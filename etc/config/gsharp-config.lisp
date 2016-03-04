(in-package :cl-desktop)

;;(ql:quickload "gsharp")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (gsharp:gsharp)))
				    


