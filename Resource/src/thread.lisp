(in-package :desktop-internals)

;;;
;;; Clim resource for threads
;;;


;;; presentation

(clim:define-presentation-type thread ())

(clim:define-presentation-method clim:presentation-typep (object (type thread))
  #+sbcl (typep object 'sb-thread:thread)
  #-sbcl t)

(clim:define-presentation-method clim:present (object (type thread) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (bt:thread-name object) stream))

(clim:define-presentation-method clim:accept ((type thread) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (mapcar #'(lambda (o)
		 (clim:suggest (bt:thread-name o) o))
	     (bt:all-threads)))))

;;; command table

(clim:define-command-table thread-command-table)

;;; translators


(clim:define-presentation-translator expression-to-thread
    (clim:expression thread thread-command-table
		     :documentation "expression to thread"
		     :tester ((object) (clim:presentation-typep object 'thread))
		     :tester-definitive t)
    (object)
  object)

(clim:define-presentation-translator thread-to-expression
    (thread clim:expression thread-command-table
		     :documentation "thread to expression"
		     :tester-definitive t)
    (object)
  object)

;;; commands

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

;;; debugging

(clim:define-command (com-thread-inspect  :command-table thread-command-table
					  :name "Inspect thread")
    ((thread 'thread :prompt "which thread?"))
  (launch-application (find-application "clouseau")
		      :args (list thread)))

(clim:define-command (com-thread-describe  :command-table thread-command-table
					   :name "Describe thread")
    ((thread 'thread :prompt "which thread?"))
  (climi::describe thread *query-io*))

;;; show

(clim:define-command (com-thread-show-all :command-table thread-command-table
					  :menu nil
					  :name "Show threads")
    ()
  (show-resources (bt:all-threads) *query-io*))

(clim:define-command (com-thread-show-current :command-table thread-command-table
					      :name "Show current thread")
    ()
  (show-resource (bt:current-thread) *query-io*))


;;; UTILITY

#+sbcl
(defmethod desktop-presentation-type-of ((thread sb-thread:thread))
  'thread)
