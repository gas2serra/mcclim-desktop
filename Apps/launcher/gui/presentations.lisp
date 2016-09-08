(in-package :mcclim-desktop-launcher)

(clim:define-presentation-method clim:present (app (type application) stream (view clim:textual-view) &key)
  (with-accessors ((label mcclim-desktop-core:application-pretty-name)) app
    (format stream "~A~%" label)))
