(in-package :mcclim-desktop)

(setf (application-entry-fn *application*) #'(lambda(&rest args) 
                                               (ernestine-gui:player)))
      
