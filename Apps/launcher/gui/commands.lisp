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
    (clim:window-clear pane)))      




;; traslators

(clim:define-presentation-to-command-translator launch-app
    (application deski::com-launch-app desktop-launcher
		 :documentation "launch app"
		 :gesture :select
		 :tester ((app) (not (application-requires-args-p app))))
    (app)
  (list app))

(clim:define-presentation-to-command-translator open-app-home-page
    (application deski::com-open-app-home-page desktop-launcher
		 :gesture :help
		 :documentation "open app home page"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-def-file
    (application deski::com-edit-app-def-file desktop-launcher
		 :gesture :help
		 :documentation "edit app file"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-config-file
    (application deski::com-edit-app-config-file desktop-launcher
		 :gesture :help
		 :documentation "edit app config"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))

(clim:define-presentation-to-command-translator edit-app-style-file
    (application deski::com-edit-app-style-file desktop-launcher
		 :gesture :help
		 :documentation "edit app style"
		 :tester ((app) (declare (ignore app)) t))
    (app)
  (list app))


