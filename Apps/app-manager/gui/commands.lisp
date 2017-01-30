(in-package :desktop-app-manager)


(define-desktop-app-manager-command (com-quit  :menu nil
					       :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))


(define-desktop-app-manager-command (com-refresh  :menu nil
						  :keystroke (#\r :meta))
    ()
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))

(define-desktop-app-manager-command (com-refresh-apps  :menu nil)
    ()
  (refresh-applications)
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))

(define-desktop-app-manager-command (com-clear-interactor
				     :menu nil
				     :keystroke (#\c :meta))
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interactor)))
    (clim:window-clear pane)))




;; traslators

