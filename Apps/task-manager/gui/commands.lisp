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

;;; gesture :select and :help

(define-task-manager-command (com-application-frame-exit
			      :name "Exit frame")
    ((frame 'clim:application-frame :prompt " Which frame? "))
  (clim:frame-exit frame)
  (com-refresh))

(define-task-manager-command (com-application-frame-raise
			      :name "Raise frame")
    ((frame 'clim:application-frame :prompt " Which frame? "))
  (clim:raise-frame frame))


(define-task-manager-command (com-application-frame-inspect
			      :name "Inspect frame")
    ((frame 'clim:application-frame :prompt " Which frame? "))
  (clouseau:inspector frame))

(define-task-manager-command (com-application-frame-break
			      :name "Break frame")
    ((frame 'clim:application-frame :prompt " Which frame? "))
  (dolist (thread (clim-sys:all-processes))
    (bt:interrupt-thread thread
			 #'(lambda ()
			     (when (and (boundp 'clim:*application-frame*)
					(eq clim:*application-frame* frame))
			       (break))))))


(define-task-manager-command (com-thread-break
			      :name "Break thread")
    ((thread 'thread :prompt " Which thread? "))
  (bt:interrupt-thread thread
		       #'(lambda ()
			     (break))))

(define-task-manager-command (com-thread-destroy
			      :name "Destroy thread")
    ((thread 'thread :prompt " Which thread? "))
  (bt:destroy-thread thread)
  (com-refresh))
		  
;;
;; command traslators
;;

(clim:define-presentation-to-command-translator break-frame
    (clim:application-frame com-application-frame-break task-manager
		 :gesture :help
		 :documentation "break frame"
		 :tester ((frame) t))
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator exit-frame
    (clim:application-frame com-application-frame-exit task-manager
		 :gesture :help
		 :documentation "exit frame"
		 :tester ((frame) t))
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator raise-frame
    (clim:application-frame com-application-frame-raise task-manager
		 :gesture :help
		 :documentation "raise frame"
		 :tester ((frame) t))
    (frame)
  (list frame))

(clim:define-presentation-to-command-translator inspect-frame
    (clim:application-frame com-application-frame-inspect task-manager
		 :gesture :select
		 :documentation "inspect frame"
		 :tester ((frame) t))
    (frame)
  (list frame))


(clim:define-presentation-to-command-translator break-thread
    (thread com-thread-break task-manager
		 :gesture :help
		 :documentation "break thread"
		 :tester ((thread) t))
    (thread)
  (list thread))

(clim:define-presentation-to-command-translator destroy-thread
    (thread com-thread-destroy task-manager
		 :gesture :help
		 :documentation "destroy thread"
		 :tester ((thread) t))
    (thread)
  (list thread))
