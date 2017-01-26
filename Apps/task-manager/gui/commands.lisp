(in-package :desktop-task-manager)

;;;
;;; task manager commands
;;;

(define-task-manager-command (com-quit :name "Quit"
				       :menu t
				       :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))


(define-task-manager-command (com-refresh  :menu t
					   :name "Refresh"
					   :keystroke (#\r :meta))
    ()
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame*
						   'frame-display)
			     :force-p t)
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame*
						   'thread-display)
			     :force-p t))

(define-task-manager-command (com-clear
			      :name "Clear"
			      :keystroke (#\c :meta))
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interact)))
    (clim:window-clear pane)))

