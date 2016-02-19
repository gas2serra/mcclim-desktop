(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'(lambda (&rest args)
					       (climacs:climacs-rv)))
      
