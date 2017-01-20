(in-package :desktop-internals)

;;;
;;; Clim resource for application frames
;;;


;; presentations

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

;; command table
(clim:define-command-table frame-command-table)

;; commands

(clim:define-command (com-list-frames :command-table frame-command-table
				      :menu nil
				      :name "List frames")
    ()
  (with-resource-list-output (*standard-output*)
    (clim:map-over-frames #'(lambda (frame)
			      (with-resource-list-item-output (*standard-output*)
				(clim:present frame 'clim:application-frame
					      :view clim:+textual-view+
					      :stream *standard-output*)))))
  nil)

(clim:define-command (com-application-frame-exit :command-table frame-command-table
						 :name "Exit frame")
    ((frame 'clim:application-frame :prompt "which frame?"))
  (clim:frame-exit frame))

(clim:define-command (com-application-frame-raise :command-table frame-command-table
						  :name "Raise frame")
    ((frame 'clim:application-frame :prompt "which frame?"))
  (clim:raise-frame frame))

(clim:define-command (com-application-frame-inspect  :command-table frame-command-table
						     :name "Inspect frame")
    ((frame 'clim:application-frame :prompt "which frame?"))
  (launch-application (find-application "clouseau")
		      :args (list frame)))

(clim:define-command (com-application-frame-break :command-table frame-command-table
						  :name "Break frame")
    ((frame 'clim:application-frame :prompt "which frame?"))
  (dolist (thread (clim-sys:all-processes))
    (bt:interrupt-thread thread
			 #'(lambda ()
			     (when (and (boundp 'clim:*application-frame*)
					(eq clim:*application-frame* frame))
			       (break))))))

;; translators

(clim:define-presentation-to-command-translator break-frame
    (clim:application-frame com-application-frame-break frame-command-table
			    :gesture :help
			    :documentation "break frame")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator exit-frame
    (clim:application-frame com-application-frame-exit frame-command-table
		 :gesture :help
		 :documentation "exit frame")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator raise-frame
    (clim:application-frame com-application-frame-raise frame-command-table
		 :gesture :help
		 :documentation "raise frame")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator inspect-frame
    (clim:application-frame com-application-frame-inspect frame-command-table
			    :gesture :select
			    :documentation "inspect frame")
    (frame)
  (list frame))
