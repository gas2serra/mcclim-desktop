(in-package :desktop-internals)

;;;
;;; Clim resource for threads
;;;


;; presentations

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
	     (bt:all-threads)))))


;; command table
(clim:define-command-table thread-command-table)

;; commands

(clim:define-command (com-list-thread :command-table thread-command-table
				      :menu nil
				      :name "List thread")
    ()
  (dolist (thread (bt:all-threads))
    (fresh-line *standard-output*)
    (clim:present thread
		  'thread
		  :stream *standard-output*)))

(clim:define-command (com-thread-break :command-table thread-command-table
				       :name "Break thread")
    ((thread 'thread :prompt "which thread?"))
  (bt:interrupt-thread thread
		       #'(lambda ()
			   (break))))

(clim:define-command (com-thread-destroy :command-table thread-command-table
					 :name "Destroy thread")
    ((thread 'thread :prompt "which thread?"))
  (bt:destroy-thread thread))

;; translators

(clim:define-presentation-to-command-translator break-thread
    (thread com-thread-break thread-command-table
	    :gesture :help
	    :documentation "break thread")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator destroy-thread
    (thread com-thread-destroy thread-command-table
		 :gesture :help
		 :documentation "destroy thread")
    (thread)
  (list thread))
