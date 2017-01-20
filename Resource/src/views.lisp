(in-package :desktop-internals)


(defgeneric desktop-presentation-type-of (object)
  (:method (object)
    (clim:presentation-type-of object)))

;; view


(defclass extended-textual-view (clim:textual-view)
  ())

(defparameter +extended-textual-view+ 
  (make-instance 'extended-textual-view))

(defclass list-textual-view (clim:textual-view)
  ())

(defparameter +list-textual-view+ 
  (make-instance 'list-textual-view))

(clim:define-presentation-type list ())


(clim:define-presentation-method clim:present (list (type list)
						    stream
						    (view list-textual-view)
						    &key)
  (clim:stream-increment-cursor-position *standard-output* 0 20)
  (clim:surrounding-output-with-border
   (*standard-output*
    :shape :rounded
    :padding 10
    :ink clim:+yellow+
    :background clim:+grey80+) 
   (dolist (l list)
     (progn
       (clim:stream-increment-cursor-position *standard-output* 30 0)
       (clim:present l (desktop-presentation-type-of l)
		     :stream stream)
       (format stream "~%")))))

