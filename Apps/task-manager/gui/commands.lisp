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
  

;;; gesture :select and :help

(define-task-manager-command (com-application-frame-exit
			      :name "Exit Application Frame")
    ((frame 'clim:application-frame :gesture :help))
  (clim:frame-exit frame)
  (com-refresh))

(define-task-manager-command (com-application-frame-raise
			      :name "Raise Application Frame")
    ((frame 'clim:application-frame :gesture :help))
  (clim:raise-frame frame))


(define-task-manager-command (com-application-frame-inspect
			      :name "Inspect Application Frame")
    ((frame 'clim:application-frame :gesture :select))
  (clouseau:inspector frame))

(define-task-manager-command (com-application-frame-break
			      :name "Break Application Frame")
    ((frame 'clim:application-frame :gesture :help))
  (dolist (thread (clim-sys:all-processes))
    (bt:interrupt-thread thread
			 #'(lambda ()
			     (when (and (boundp 'clim:*application-frame*)
					(eq clim:*application-frame* frame))
			       (break))))))


(define-task-manager-command (com-thread-break
			      :name "Break Thread")
    ((thread 'thread :gesture :help))
  (bt:interrupt-thread thread
		       #'(lambda ()
			     (break))))

(define-task-manager-command (com-thread-destroy
			      :name "Destroy Thread")
    ((thread 'thread :gesture :help))
  (bt:destroy-thread thread)
  (com-refresh))
		  
