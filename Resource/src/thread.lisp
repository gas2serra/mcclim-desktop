(in-package :desktop-internals)

;;;
;;; Clim resource for threads
;;;

(defun thread-textual-string (thread)
  (bt:thread-name thread))

;;; presentation

(clim:define-presentation-type thread ())

(clim:define-presentation-method clim:presentation-typep (object (type thread))
  (bt:threadp object))

(clim:define-presentation-method clim:present (object (type thread) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (thread-textual-string object) stream))

(clim:define-presentation-method clim:accept ((type thread) stream view &key)
  (declare (ignore view))
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (mapcar #'(lambda (thread)
		 (clim:suggest (thread-textual-string thread) thread))
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

(clim:define-command (com-break-thread :command-table thread-command-table
				       :name t
				       :menu t)
    ((thread 'thread))
  (bt:interrupt-thread thread
		       #'(lambda ()
			   (break))))

(clim:define-command (com-destroy-thread :command-table thread-command-table
					 :name t
					 :menu t)
    ((thread 'thread))
  (bt:destroy-thread thread))

(clim:define-command (com-inspect-thread :command-table thread-command-table
					 :name t
					 :menu t)
    ((thread 'thread))
  (launch-application (find-application "clouseau")
		      :args (list thread)))

(clim:define-command (com-describe-thread :command-table thread-command-table
					  :name t
					  :menu t)
    ((thread 'thread))
  (climi::describe thread *query-io*))

(clim:define-command (com-show-thread :command-table thread-command-table
				      :name t
				      :menu t)
    ((thread 'thread))
  (show-resource thread *query-io*))

(clim:define-command (com-list-threads :command-table thread-command-table
				       :name t
				       :menu t)
    ()
  (list-resources (bt:all-threads) *query-io*))


;; translators

(clim:define-presentation-to-command-translator break-thread
    (thread com-break-thread thread-command-table
	    :gesture :help
	    :documentation "break")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator destroy-thread
    (thread com-destroy-thread thread-command-table
		 :gesture :help
		 :documentation "destroy")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator describe-thread
    (thread com-describe-thread thread-command-table
		 :gesture :help
		 :documentation "describe")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator inspect-thread
    (thread com-inspect-thread thread-command-table
		 :gesture :help
		 :documentation "inspect")
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator show-thread
    (thread com-show-thread thread-command-table
		 :gesture :help
		 :documentation "show")
    (thread)
  (list thread))

;;; UTILITY

#+sbcl
(defmethod desktop-presentation-type-of ((thread sb-thread:thread))
  'thread)
