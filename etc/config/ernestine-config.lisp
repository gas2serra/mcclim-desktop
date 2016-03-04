(in-package :cl-desktop)

;;(ql:quickload "ernestine-gui")

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (ernestine-gui:player)))


