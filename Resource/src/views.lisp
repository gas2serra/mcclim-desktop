(in-package :desktop-internals)

;; view

(defclass extended-textual-view (clim:textual-view)
  ())

(defparameter +extended-textual-view+ 
  (make-instance 'extended-textual-view))
