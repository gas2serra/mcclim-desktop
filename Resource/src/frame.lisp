(in-package :desktop-internals)

;;;
;;; Clim resource for application frames
;;;


(defun frame-textual-string (frame)
  (if (typep frame 'clim:standard-application-frame)
      (clim:frame-pretty-name frame)
      (format nil "~A" frame)))

;; presentations

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (frame-textual-string object) stream))

(clim:define-presentation-method clim:accept ((type clim:application-frame) stream view &key)
  (declare (ignore view))
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (clim:map-over-frames #'(lambda (o)
			       (clim:suggest (frame-textual-string o) o))))))

(clim:define-presentation-method clim:present
    (object (type clim:application-frame) stream
	    (view extended-textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format stream "~A ~{(~A,~A) [~Ax~A]~}"
	  (frame-textual-string object)
	  (clim:with-bounding-rectangle* (x1 y1 x2 y2)  
	      (clim:transform-region (clim:sheet-transformation
				      (clim:frame-top-level-sheet object))
				     (clim:sheet-region
				      (clim:frame-top-level-sheet object)))
	    (list x1 y1 (- x2 x1) (- y2 y1)))))

;; command table

(clim:define-command-table frame-command-table)

;;; translators

(clim:define-presentation-translator expression-to-frame
    (clim:expression clim:application-frame frame-command-table
		     :documentation "expression to frame"
		     :tester ((object) (clim:presentation-typep object 'clim:application-frame))
		     :tester-definitive t)
    (object)
  object)

(clim:define-presentation-translator frame-to-expression
    (frame clim:expression frame-command-table
		     :documentation "frame to expression"
		     :tester-definitive t)
    (object)
  object)

;; commands

(clim:define-command (com-exit-frame :command-table frame-command-table
				     :name t
				     :menu t)
    ((frame 'clim:application-frame))
  (clim:frame-exit frame))

(clim:define-command (com-raise-frame :command-table frame-command-table
				      :name t
				      :menu t)
    ((frame 'clim:application-frame))
  (clim:raise-frame frame))

(clim:define-command (com-inspect-frame :command-table frame-command-table
					:name t
					:menu t)
    ((frame 'clim:application-frame))
  (launch-application (find-application "clouseau")
		      :args (list frame)))

(clim:define-command (com-break-frame :command-table frame-command-table
				      :name t
				      :menu t)
    ((frame 'clim:application-frame))
  (dolist (thread (clim-sys:all-processes))
    (bt:interrupt-thread thread
			 #'(lambda ()
			     (when (and (boundp 'clim:*application-frame*)
					(eq clim:*application-frame* frame))
			       (break))))))


(clim:define-command (com-describe-frame :command-table frame-command-table
					 :name t
					 :menu t)
    ((frame 'clim:application-frame))
  (climi::describe frame *query-io*))

(clim:define-command (com-show-frame :command-table frame-command-table
				     :name t
				     :menu t)
    ((frame 'clim:application-frame))
  (show-resource frame *query-io*))

(clim:define-command (com-list-frames :command-table frame-command-table
				      :name t
				      :menu t)
    ()
  (let ((frames nil))
    (clim:map-over-frames #'(lambda (f) (push f frames)))
    (list-resources frames *query-io*)))

;; translators

(clim:define-presentation-to-command-translator break-frame
    (clim:application-frame com-break-frame frame-command-table
			    :gesture :help
			    :documentation "break")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator exit-frame
    (clim:application-frame com-exit-frame frame-command-table
			    :gesture :help
			    :documentation "exit")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator raise-frame
    (clim:application-frame com-raise-frame frame-command-table
			    :gesture :help
			    :documentation "raise")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator inspect-frame
    (clim:application-frame com-inspect-frame frame-command-table
			    :gesture :select
			    :documentation "inspect")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator describe-frame
    (clim:application-frame com-describe-frame frame-command-table
			    :gesture :select
			    :documentation "describe")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator show-frame
    (clim:application-frame com-show-frame frame-command-table
			    :gesture :select
			    :documentation "show")
    (frame)
  (list frame))
