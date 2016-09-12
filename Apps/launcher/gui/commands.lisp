(in-package :mcclim-desktop-launcher)


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

(define-desktop-launcher-command (com-clear-log
				  :name "Clear Log"
				  :keystroke (#\c :meta))
    ()
  (clim:window-clear
   (clim:find-pane-named clim:*application-frame* 'log-display)))

(define-desktop-launcher-command (com-launch-app :name "Launch App")
		
    ((app 'application :gesture :select))
  (launch-application app))

(define-desktop-launcher-command (com-open-home-page :name "Open Home Page")
    ((app 'application :gesture :help))
  (launch-application (find-application "browser")
		      :args (list (application-home-page app))))

(define-desktop-launcher-command (com-edit-app-file :name "Edit App")
    ((app 'application :gesture :help))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-file app t force-user-p)
      (edit-file (application-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (refresh-application (application-name app)))))))

(define-desktop-launcher-command (com-edit-config-file :name "Edit Config")
    ((app 'application :gesture :help))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-config-file app t force-user-p)
      (edit-file (application-config-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (when (application-configured-p app)
			      (configure-application app t)))))))

(define-desktop-launcher-command (com-edit-style-file :name "Edit Style")
    ((app 'application :gesture :help))
  (with-slots (force-user-p) clim:*application-frame*
    (when (application-style-file app t force-user-p)
      (edit-file (application-style-file app)
		 :cb-fn #'(lambda (&rest rest)
			    (declare (ignore rest))
			    (when (application-configured-p app)
			      (configure-application app t)))))))

#|

;;
;; command traslators
;;

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app desktop-launcher
		 :gesture :select
		 :documentation "Launch")
    (object) (list object))

(clim:define-presentation-to-command-translator refresh-application
    (application com-refresh-application desktop-launcher
		       :gesture :help
		       :documentation "Refresh")
  (object) (list object))

(clim:define-presentation-to-command-translator configure-application
    (application com-configure-application desktop-launcher
		 :gesture :help
		 :documentation "Configure")
  (object) (list object))


(clim:define-presentation-to-command-translator edit-application-file
    (application com-edit-application-file desktop-launcher
		 :gesture :help
		 :documentation "Edit application file")
    (object) (list object))

(clim:define-presentation-to-command-translator edit-config-file
    (application com-edit-config-file desktop-launcher
		 :gesture :help
		 :documentation "Edit config file")
    (object) (list object))

(clim:define-presentation-to-command-translator open-home-page
    (application com-open-home-page desktop-launcher
		 :gesture :help
		 :documentation "Open home page")
    (object) (list object))
|#
