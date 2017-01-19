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




