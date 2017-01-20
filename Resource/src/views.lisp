(in-package :desktop-internals)


(defgeneric desktop-presentation-type-of (object)
  (:method (object)
    (clim:presentation-type-of object)))

;; view

(defclass extended-textual-view (clim:textual-view)
  ())

(defparameter +extended-textual-view+ 
  (make-instance 'extended-textual-view))
