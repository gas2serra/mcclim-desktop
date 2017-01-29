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

(define-desktop-app-manager-command (com-show-app
				     :menu nil
				     :name "Show")
    ((app 'application))
  (fresh-line)
  (clim:present app))


(clim:define-presentation-to-command-translator show-app
    (application com-show-app desktop-app-manager
		 :documentation "show app"
		 :gesture :select)
    (app)
  (list app))



