(in-package :desktop-internals)

;;;
;;; Clim resource for application frames
;;;

(defun frame->textual-string (frame)
  (if (typep frame 'clim:standard-application-frame)
      (clim:frame-pretty-name frame)
      (format nil "~A" frame)))

;; presentations

(clim:define-presentation-type frame ())

(clim:define-presentation-method clim:presentation-typep (object (type frame))
  (typep object 'clim:application-frame))

(clim:define-presentation-method clim:present
    (object (type frame) stream
	    (view clim:textual-view)
	    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (frame->textual-string object) stream))

(clim:define-presentation-method clim:accept ((type frame) stream view &key)
  (declare (ignore view))
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (clim:map-over-frames #'(lambda (o)
			       (clim:suggest (frame->textual-string o) o))))))

;; command table

(clim:define-command-table frame-command-table)

;;; translators

(clim:define-presentation-translator expression-to-frame
    (clim:expression frame frame-command-table
		     :documentation "expression to frame"
		     :tester ((object) (clim:presentation-typep object 'frame))
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
    ((frame 'frame))
  (clim:frame-exit frame))

(clim:define-command (com-raise-frame :command-table frame-command-table
				      :name t
				      :menu t)
    ((frame 'frame))
  (clim:raise-frame frame))

(clim:define-command (com-inspect-frame :command-table frame-command-table
					:name t
					:menu t)
    ((frame 'frame))
  (launch-application (find-application "clouseau")
		      :args (list frame)))

(clim:define-command (com-break-frame :command-table frame-command-table
				      :name t
				      :menu t)
    ((frame 'frame))
  (when (and
	 (typep frame 'clim:standard-application-frame)
	 (climi::frame-process frame))
    (bt:interrupt-thread (climi::frame-process frame)
			 #'(lambda ()
			     (break)))))

(clim:define-command (com-list-frames :command-table frame-command-table
				      :name nil
				      :menu nil)
    ()
  (clim:map-over-frames
   #'(lambda (frame)
       (fresh-line)
       (clim:with-output-as-presentation (t frame
					    (deski::desktop-presentation-type-of frame)
					    :allow-sensitive-inferiors nil
					    :single-box t)
	 (clim:present frame 'clim:expression)))))

;; translators

(clim:define-presentation-to-command-translator break-frame
    (frame com-break-frame frame-command-table
			    :gesture :help
			    :tester ((object) (climi::frame-process object))
			    :documentation "break")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator exit-frame
    (frame com-exit-frame frame-command-table
			    :gesture :help
			    :documentation "exit")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator raise-frame
    (frame com-raise-frame frame-command-table
			    :gesture :help
			    :tester ((object) (climi::frame-process object))
			    :documentation "raise")
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator inspect-frame
    (frame com-inspect-frame frame-command-table
			    :gesture :select
			    :documentation "inspect")
    (frame)
  (list frame))

(defmethod desktop-presentation-type-of ((frame clim:application-frame))
  'frame)
