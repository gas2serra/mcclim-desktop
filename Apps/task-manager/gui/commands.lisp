(in-package :desktop-task-manager)

;;;
;;; task manager commands
;;;

(defun mem-usage ()
  #+(or cmu scl) (lisp::dynamic-usage)
  #+sbcl  (sb-kernel:dynamic-usage)
  #+lispworks (getf (system:room-values) :total-allocated)
  #+openmcl (+ (ccl::%usedbytes) (ccl::%freebytes))
  #+clisp (values (sys::%room))
  #-(or cmu scl sbcl lispworks openmcl clisp) 0)


(define-task-manager-command (com-quit :name "Quit"
				       :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))


(define-task-manager-command (com-refresh  :name "Refresh"
					   :keystroke (#\r :meta))
    ()
  (with-slots (thread-num-history memory-usage-history history-position)
      clim:*application-frame*
    (setf (elt thread-num-history history-position) (length (bt:all-threads)))
    (setf (elt memory-usage-history history-position) (mem-usage))
    (setf history-position (mod (+ 1 history-position) history-size)))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'thread-display)))
    (clim:change-space-requirements pane :width 0 :height 0
				    :min-width 0 :min-height 0)
    (clim:redisplay-frame-pane clim:*application-frame*
			       pane
			       :force-p t))
  (let ((pane (clim:find-pane-named clim:*application-frame* 'history-display)))
    (clim:change-space-requirements pane :width 0 :height 0
				    :min-width 0 :min-height 0)
    (clim:redisplay-frame-pane clim:*application-frame*
			       pane
			       :force-p t)))
  

(define-task-manager-command (com-clear-interactor
			      :name "Clear interactor history"
			      :keystroke (#\c :meta))
    ()
  (let ((pane (clim:find-pane-named clim:*application-frame* 'interact)))
    (clim:window-clear pane)))

