(in-package :desktop-launcher)


(define-desktop-launcher-command (com-quit  :menu t
					    :name "Quit"
					    :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))

(define-desktop-launcher-command (com-refresh  :menu t
					       :name "Refresh"
					       :keystroke (#\r :meta))
    ()
  (refresh-applications)
  (update-applications))

(define-desktop-launcher-command (com-clear
				  :name "Clear"
				  :keystroke (#\c :meta))
    ()
  (let ((pane (or (clim:find-pane-named clim:*application-frame* 'log-display)
		  (clim:find-pane-named clim:*application-frame* 'interactor))))
    (clim:window-clear pane)
    (clim:redisplay-frame-pane clim:*application-frame* pane :force-p t)))

(define-desktop-launcher-command (com-launch-app :name "Launch App")	
    ((app application :prompt " Which app? "))
  (launch-application app))

(define-desktop-launcher-command (com-open-home-page :name "Open Home Page")
    ((app 'application :prompt " Which app? "))
  (launch-application (find-application "browser")
		      :args (list (application-home-page app))))

(define-desktop-launcher-command (com-edit-app-file :name "Edit App")
    ((app 'application :prompt " Which app? "))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-file app t force-user-p)
      (refresh-application (application-name app))
      (edit-file (application-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (refresh-application (application-name app)))))))

(define-desktop-launcher-command (com-edit-config-file :name "Edit Config")
    ((app 'application :prompt " Which app? "))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-config-file app t force-user-p)
      (when (application-configured-p app)
	(configure-application app t))
      (edit-file (application-config-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (when (application-configured-p app)
			      (configure-application app t)))))))

(define-desktop-launcher-command (com-edit-style-file :name "Edit Style")
    ((app 'application :prompt " Which app? "))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-configured-p app)
      (configure-application app t))
    (when (application-style-file app t force-user-p)
      (edit-file (application-style-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (when (application-configured-p app)
			      (configure-application app t)))))))

;;
;; command traslators
;;

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app desktop-launcher
		 :gesture :select
		 :documentation "launch"
		 :tester ((app) (not (application-requires-args-p app))))
    (app)
  (list app))

(clim:define-presentation-to-command-translator open-home-page
    (application com-open-home-page desktop-launcher
		 :gesture :help
		 :documentation "open home page"
		 :tester ((app) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-file
    (application com-edit-app-file desktop-launcher
		 :gesture :help
		 :documentation "edit app file"
		 :tester ((app) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-config-file
    (application com-edit-config-file desktop-launcher
		 :gesture :help
		 :documentation "edit config file"
		 :tester ((app) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-style-file
    (application com-edit-style-file desktop-launcher
		 :gesture :help
		 :documentation "edit style file"
		 :tester ((app) t))
    (app)
  (list app))

