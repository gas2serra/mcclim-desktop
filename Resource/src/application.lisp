(in-package :desktop-internals)

;;;
;;; Clim resource for applications
;;;

;; presentations


(clim:define-presentation-method clim:present (app (type application)
						   stream
						   (view clim:textual-view)
						   &key)
  (with-accessors ((label application-pretty-name)) app
    (write-string label stream)))

(clim:define-presentation-method clim:accept ((type application) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
				     (mapcar #'(lambda (o)
						 (clim:suggest (application-pretty-name o) o))
					     (registered-applications)))))

;; command table
(clim:define-command-table desktop-application-command-table)

;; parameters

(defvar *force-user-app-files-p* t)

;; utility

(defun edit-file (filename &key cb-fn)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename) :cb-fn cb-fn)))

;; commands

(clim:define-command (com-list-apps :command-table desktop-application-command-table
				     :menu nil
				     :name "List apps")
    ((all-p boolean :default nil :prompt "all apps?"))
  (let ((apps nil))
    (dolist (app (registered-applications))
      (when (or all-p (application-menu-p app))
	(push app apps)))
    (clim:present apps
		  'list
		  :view +list-textual-view+ :stream *standard-output*))
  nil)

(clim:define-command (com-launch-app :command-table desktop-application-command-table
				     :menu nil
				     :name "Launch app")
    ((app application :prompt "which app?"))
  (launch-application app))
  
(clim:define-command (com-open-app-home-page :command-table desktop-application-command-table
					     :menu nil
					     :name "Open app home page")
    ((app 'application :prompt "which app?"))
  (launch-application (find-application "browser")
		      :args (list (application-home-page app))))


(clim:define-command (com-edit-app-def-file :command-table desktop-application-command-table
					    :menu nil
					    :name "Edit app def")
    ((app 'application :prompt "which app?"))
  (when (application-file app t *force-user-app-files-p*)
    (refresh-application (application-name app))
    (edit-file (application-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (refresh-application (application-name app))))))

(clim:define-command (com-edit-app-config-file :command-table desktop-application-command-table
					       :menu nil
					       :name "Edit app config")
    ((app 'application :prompt "which app?"))
  (when (application-config-file app t *force-user-app-files-p*)
    (when (application-configured-p app)
      (configure-application app t))
    (edit-file (application-config-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (when (application-configured-p app)
			    (configure-application app t))))))

(clim:define-command (com-edit-app-style-file :command-table desktop-application-command-table
					      :name "Edit app style")
    ((app 'application :prompt "which app?"))
  (when (application-configured-p app)
    (configure-application app t))
  (when (application-style-file app t *force-user-app-files-p*)
    (edit-file (application-style-file app)
	       :cb-fn #'(lambda (&rest rest)
			  (declare (ignore rest))
			  (when (application-configured-p app)
			    (configure-application app t))))))
