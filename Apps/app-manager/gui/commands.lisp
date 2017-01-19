(in-package :desktop-app-manager)


(define-desktop-app-manager-command (com-quit  :menu t
					    :name "Quit"
					    :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))

(define-desktop-app-manager-command (com-refresh  :menu t
					       :name "Refresh"
					       :keystroke (#\r :meta))
    ()
  (refresh-applications)
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))

(define-desktop-app-manager-command (com-clear
				  :name "Clear"
				  :keystroke (#\c :meta))
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interactor)))
    (clim:window-clear pane)))




