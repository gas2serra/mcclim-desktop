(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'clim-listener:run-listener)
      
