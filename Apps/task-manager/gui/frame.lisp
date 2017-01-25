(in-package :desktop-task-manager)


(clim:define-application-frame task-manager (clim:standard-application-frame)
  ((timer))
  (:panes
   (frame-display :application
		  :display-function #'%render-frame-display
		  :display-time nil)
   (thread-display :application
		   :display-function #'%render-thread-display
		   :display-time nil)
   (doc :pointer-documentation) 
   (interact :interactor))
  (:command-table
   (task-manager :inherit-from (deski::frame-command-table
				deski::thread-command-table)))
  (:menu-bar t)
  (:layouts (default
		(clim:vertically ()
		  (2/3
		   (clim:horizontally nil
		     (clim:labelling (:label "Frames")
		       frame-display)
		     (clim:labelling (:label "Threads")
		       thread-display)))
		  (1/3
		   (clim:labelling (:label "Interactor")
		     interact))
		  doc))))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame task-manager))
  (declare (ignore fm))
  (with-slots (timer) frame
    (setf timer (trivial-timers:make-timer
		 #'(lambda ()
		     (clim:execute-frame-command frame '(com-refresh)))))
    (trivial-timers:schedule-timer timer 3.0 :repeat-interval 3.0)))

(defmethod clim:disown-frame  :after (fm (frame task-manager))
  (declare (ignore fm))
  (with-slots (timer) frame
    (trivial-timers:unschedule-timer timer)))

;;;
;;; render functions
;;;

(defun %render-frame-display (frame pane)
  (declare (ignore frame))
  (clim:map-over-frames #'(lambda (frame)
			    (fresh-line pane)
			    (clim:stream-increment-cursor-position pane 5 3)
			    (clim:present frame
					  'clim:application-frame
					  :view deski::+extended-textual-view+
					  :stream pane))))

(defun %render-thread-display (frame pane)
  (declare (ignore frame))
  (dolist (thread (bt:all-threads))
    (fresh-line pane)
    (clim:stream-increment-cursor-position pane 5 3)
    (clim:present thread 'deski::thread
		  :view clim:+textual-view+
		  :stream pane)))

