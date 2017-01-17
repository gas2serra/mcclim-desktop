(in-package :desktop-task-manager)

(defclass extended-textual-view (clim:textual-view) () )
(defparameter +extended-textual-view+ 
  (make-instance 'extended-textual-view))

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A" (clim:frame-pretty-name object)))

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view extended-textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A ~{(~A,~A) [~Ax~A]~}"
	  (clim:frame-pretty-name object)
	  (clim:with-bounding-rectangle* (x1 y1 x2 y2)  
	      (clim:transform-region (clim:sheet-transformation
				      (clim:frame-top-level-sheet object))
				     (clim:sheet-region
				      (clim:frame-top-level-sheet object)))
	    (list x1 y1 (- x2 x1) (- y2 y1)))))

(clim:define-presentation-method clim:accept ((type clim:application-frame) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (clim:map-over-frames #'(lambda (o)
			       (clim:suggest (format nil "~A" (clim:frame-pretty-name o)) o))))))

(clim:define-presentation-type thread ())

(clim:define-presentation-method clim:present (object (type thread) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (write-string (bt:thread-name object) stream))


(clim:define-presentation-method clim:accept ((type thread) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
				     (mapcar #'(lambda (o)
						 (clim:suggest (bt:thread-name o) o))
					      (clim-sys:all-processes)))))
