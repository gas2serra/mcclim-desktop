(in-package :desktop-app-manager)


(define-desktop-app-manager-command (com-quit  :menu nil
					       :name "Quit"
					       :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))

(define-desktop-app-manager-command (com-refresh-apps  :menu nil
						       :name "Refresh"
						       :keystroke (#\r :meta))
    ()
  (refresh-applications)
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))

(define-desktop-app-manager-command (com-clear
				     :menu nil
				     :name "Clear"
				     :keystroke (#\c :meta))
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interactor)))
    (clim:window-clear pane)))




;; traslators

(clim:define-presentation-to-command-translator show-app
    (application deski::com-show-app desktop-app-manager
		 :documentation "show app"
		 :gesture :select)
    (app)
  (list app))

(clim:define-presentation-to-command-translator launch-app
    (application deski::com-launch-app desktop-app-manager
		 :documentation "launch app"
		 :gesture :help
		 :tester ((app) (not (application-requires-args-p app))))
    (app)
  (list app))

(clim:define-presentation-to-command-translator open-app-home-page
    (application deski::com-open-app-home-page desktop-app-manager
		 :gesture :help
		 :documentation "open app home page"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-def-file
    (application deski::com-edit-app-def-file desktop-app-manager
		 :gesture :help
		 :documentation "edit app file"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-config-file
    (application deski::com-edit-app-config-file desktop-app-manager
		 :gesture :help
		 :documentation "edit app config"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-style-file
    (application deski::com-edit-app-style-file desktop-app-manager
		 :gesture :help
		 :documentation "edit app style"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))


