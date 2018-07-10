(in-package :desktop-internals)

;;;
;;; Clim resource for applications
;;;

(defun application->textual-string (app)
  (application-pretty-name app))

;;; presentations

(clim:define-presentation-method clim:present (app (type application)
						   stream
						   (view clim:textual-view)
						   &key)
  (princ (application->textual-string app) stream))

(clim:define-presentation-method clim:accept ((type application) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
				     (mapcar #'(lambda (o)
						 (clim:suggest (application->textual-string o) o))
					     (registered-applications)))))

;;; command table

;;(defparameter application-command-table nil)
(clim:define-command-table application-command-table)
(clim:define-command-table edit-application-command-table
    :inherit-from (application-command-table)
    :inherit-menu t)

;;; translators

(clim:define-presentation-translator expression-to-application
    (clim:expression application application-command-table
		     :documentation "expression to application"
		     :tester ((object) (clim:presentation-typep object 'application))
		     :tester-definitive t)
    (object)
  object)

(clim:define-presentation-translator application-to-expression
    (application clim:expression application-command-table
	    :documentation "application to expression")
    (object)
  object)

;;; parameters

(defvar *force-user-app-files-p* t)

;;; commands

(clim:define-command (com-launch-app :command-table application-command-table
				     :name t
				     :menu t)
    ((app application))
  (launch-application app))
  
(clim:define-command (com-open-app-home-page :command-table application-command-table
					     :name t
					     :menu t)
    ((app 'application))
  (launch-application (find-application "browser")
		      :args (list (application-home-page app))))

(clim:define-command (com-inspect-app :command-table application-command-table
				      :name t
				      :menu t)
    ((app 'application))
  (launch-application (find-application "clouseau")
		      :args (list app)))

(clim:define-command (com-edit-app-def-file :command-table edit-application-command-table
					    :menu t
					    :name t)
    ((app 'application))
  (when (application-file app t *force-user-app-files-p*)
    (refresh-application (application-name app))
    (edit-file (application-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (refresh-application (application-name app))))))

(clim:define-command (com-edit-app-config-file :command-table edit-application-command-table
					       :menu t
					       :name t)
    ((app 'application))
  (when (application-config-file app t *force-user-app-files-p*)
    (when (application-configured-p app)
      (configure-application app t))
    (edit-file (application-config-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (when (application-configured-p app)
			    (configure-application app t))))))

(clim:define-command (com-edit-app-style-file :command-table edit-application-command-table
					      :name t
					      :menu t)
    ((app 'application))
  (when (application-configured-p app)
    (configure-application app t))
  (when (application-style-file app t *force-user-app-files-p*)
    (edit-file (application-style-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (when (application-configured-p app)
			    (configure-application app t))))))


(clim:define-command (com-list-apps :command-table application-command-table
				    :name nil
				    :menu nil)
    ()
  (dolist (app (registered-applications))
    (fresh-line)
    (clim:with-output-as-presentation (t app (deski::desktop-presentation-type-of app)
					 :allow-sensitive-inferiors nil
					 :single-box t)
      (clim:present app 'clim:expression))))

;; utility

(defun edit-file (filename &key cb-fn)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename) :cb-fn cb-fn)))

;;; translators

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app application-command-table
	    :gesture :help
	    :documentation "launch")
    (app)
  (list app))

(clim:define-presentation-to-command-translator inspect-app
    (application com-inspect-app application-command-table
	    :gesture :help
	    :documentation "inspect")
    (app)
  (list app))

(clim:define-presentation-to-command-translator open-app-home-page
    (application com-open-app-home-page application-command-table
	    :gesture :help
	    :documentation "open home page")
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-def-file
    (application com-edit-app-def-file edit-application-command-table
		 :gesture :help
		 :documentation "edit def file")
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-config-file
    (application com-edit-app-config-file edit-application-command-table
		 :gesture :help
		 :documentation "edit config file")
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-style-file
    (application com-edit-app-style-file edit-application-command-table
	    :gesture :help
	    :documentation "edit style file")
    (app)
  (list app))

