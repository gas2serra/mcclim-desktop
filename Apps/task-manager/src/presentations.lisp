(in-package :desktop-task-manager)

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A (~A) [~{~A,~A ~Ax~A~}]"
	  (clim:frame-pretty-name object)
	  (clim:frame-name object)
	  (clim:with-bounding-rectangle* (x1 y1 x2 y2)  
	      (clim:transform-region (clim:sheet-transformation
				      (clim:frame-top-level-sheet object))
				     (clim:sheet-region
				      (clim:frame-top-level-sheet object)))
	    (list x1 y1 (- x2 x1) (- y2 y1)))))

(clim:define-presentation-type thread ())

(clim:define-presentation-method clim:present
    (object (type thread) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A"
	  (bt:thread-name object)))

