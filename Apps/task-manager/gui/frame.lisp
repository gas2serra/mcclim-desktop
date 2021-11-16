(in-package :desktop-task-manager)

(defparameter history-size 100)

(clim:define-application-frame task-manager (clim:standard-application-frame)
  ((timer)
   (thread-num-history :initform (make-array history-size))
   (memory-usage-history :initform (make-array history-size))
   (history-position :initform 0))
  (:panes
   (thread-display :application
		   :display-function #'%render-thread-display
		   :display-time nil)
   (history-display-container (clim:make-clim-stream-pane :name 'history-display
							  ;;:record nil
							  ;;draw t
							  :scroll-bars :vertical
							  :display-function #'%render-history-display
							  :display-time nil))
   (doc :pointer-documentation) 
   (interact :interactor))
  (:command-table
   (task-manager :inherit-from (deski::frame-command-table
				deski::thread-command-table)
		 :menu (("Quit" :command (com-quit))
			("TaskMan" :menu menubar-task-command-table)
			("Thread" :menu deski::thread-command-table)
			("Frame" :menu deski::frame-command-table))))
  (:menu-bar t)
  (:layouts (default
		(clim:vertically ()
		  (1/4
		   (clim:horizontally nil
		     (clim:labelling (:label "History")
		       history-display-container)))
		  (2/4
		   (clim:horizontally nil
		     (clim:labelling (:label "Threads")
		       thread-display)))
		  (1/4
		   (clim:labelling (:label "Interactor")
		     interact))
		  doc))))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame task-manager))
  (declare (ignore fm))
  (with-slots (timer) frame
    (setf timer (trivial-timers:make-timer
		 #'(lambda ()
		     (clim:execute-frame-command frame '(com-refresh)))))))

(defmethod clim:disown-frame  :after (fm (frame task-manager))
  (declare (ignore fm))
  (with-slots (timer) frame
    (when (trivial-timers:timer-scheduled-p timer)
      (trivial-timers:unschedule-timer timer))))

;;;
;;;
;;;

(defmethod clim:read-frame-command :around ((frame task-manager) &key &allow-other-keys)
  (with-slots (timer) frame
    (unwind-protect
	(progn
	  (trivial-timers:schedule-timer timer 3.0 :repeat-interval 3.0)
	  (call-next-method))
      (trivial-timers:unschedule-timer timer))))

;;;
;;; render functions
;;

(defparameter memory-usage-ink clim:+dark-green+)
(defparameter thread-num-ink clim:+dark-orange+)

(defun %render-thread-display (frame stream)
  (declare (ignore frame))
  (draw-thread-table stream)
  (fresh-line stream))

(defun %render-history-display (frame stream)
  (declare (ignore frame))
 
  (with-slots (thread-num-history)
      clim:*application-frame*
    (when thread-num-history
      (fresh-line stream)
      (draw-history stream)
      (format stream "~%")
      (draw-summary stream))))

(defun draw-summary (stream)
  (with-slots (thread-num-history memory-usage-history history-position)
      clim:*application-frame*
    (when thread-num-history
      (fresh-line stream)
      (clim:with-drawing-options (stream :ink memory-usage-ink)
	(format stream "  Memory usage: ~A    " (bytes-to-string (elt memory-usage-history (mod (1- history-position) history-size)))))
      (clim:with-drawing-options (stream :ink thread-num-ink)
	(format stream "Number of threads: ~A    " (elt thread-num-history (mod (1- history-position) history-size))))
      (clim:with-drawing-options (stream)
	;;(format stream "History length: ~A~%" (length thread-num-history)))
	)
      (fresh-line stream)))
  (clim:stream-force-output stream))

(defun draw-history (stream)
  (with-slots (thread-num-history memory-usage-history history-position)
      clim:*application-frame*
    (unless (equal
             (clim:sheet-region stream)
             climi::+nowhere+)
    (let* ((padding 10)
	   (width (- (clim:rectangle-width
		      (clim:sheet-region stream))
		     (* 2 padding)))
	   (height (- (clim:rectangle-height
		       (clim:sheet-region stream))
		      (* 2 padding)
		      20)))
      (clim:with-room-for-graphics (stream :first-quadrant nil)
	(clim:draw-rectangle* stream padding padding (+ width padding) (+ height padding) :ink clim:+grey70+)
	(draw-history-data stream width height padding thread-num-history history-position thread-num-ink)
	(draw-history-data stream width height padding memory-usage-history history-position memory-usage-ink))))
  (clim:stream-force-output stream)))

(defun draw-history-data (stream width height padding data pos ink)
  (let ((N history-size)
	(max-val (max 1 (reduce #'max data))))
    (let ((dx (/ width N))
	  (dy (/ height max-val))
	  (x padding)
	  (old-v (elt data pos))
	  (v 0))
      (loop for i from pos to (1- history-size)
	 do
	   (setf v (elt data i))
	   (clim:draw-line* stream
			    x (+ padding (- height (* old-v dy)))
			    (+ x dx) (+ padding (- height (* v dy)))
			    :line-thickness 2
			    :ink ink)
	   (setf old-v v)
	   (setf x (+ x dx)))
      (loop for i from 0 to (1- pos)
	 do
	   (setf v (elt data i))
	   (clim:draw-line* stream
			    x (+ padding (- height (* old-v dy)))
			    (+ x dx) (+ padding (- height (* v dy)))
			    :line-thickness 2
			    :ink ink)
	   (setf old-v v)
	   (setf x (+ x dx))))))
      
	   

(defun draw-thread-table (stream)
  (let ((max-width (round
		    (/ (/ (clim:rectangle-width
			   (clim:sheet-region stream))
			  2)
		       (clim:stream-string-width stream #\M)))))
    (clim:with-drawing-options (stream :text-size :large)
      (format stream "   Running frames~%~%")
      (fresh-line stream))
    (clim:formatting-table (stream :x-spacing '(2 :character))
      (clim:formatting-row (stream)
	(clim:with-text-face (stream :italic)
	  (clim:formatting-cell (stream :align-x :center) (format stream "Thread"))
	  (clim:formatting-cell (stream :align-x :center) (format stream "Frames"))
	  (clim:formatting-cell (stream :align-x :center) (format stream "Frame Position"))))
      (dolist (thread (bt:all-threads))
	(clim:formatting-row (stream)
	  (clim:formatting-cell (stream :align-x :left :align-y :top)
	    (clim:with-output-as-presentation (stream thread 'deski::thread)
	      (let ((str (clim:present-to-string thread 'deski::thread
						 :view clim:+textual-view+)))
		(if (> (length str) max-width)
		    (princ (subseq str 0 max-width) stream)
		    (princ str stream)))))
	  (clim:formatting-cell (stream :align-x :left :align-y :top)
	    (dolist (frame (deski::thread-frames thread))
	      (fresh-line stream)
	      (clim:present frame 'deski::frame
			    :view clim:+textual-view+
			    :stream stream)))
	  (clim:formatting-cell (stream :align-x :center)
	    (dolist (frame (deski::thread-frames thread))
	      (fresh-line stream)
	      (draw-small-frame-window stream frame))))))
    (fresh-line stream)
    (clim:with-drawing-options (stream :text-size :large)
      (format stream "   Sleeping frames~%~%")
      (fresh-line stream))
    (when (deski::thread-frames nil)
      (clim:formatting-table (stream :x-spacing '(2 :character))
	(clim:formatting-row (stream)
	  (clim:with-text-face (stream :italic)
	    (clim:formatting-cell (stream :align-x :center :min-width 100) (format stream "Frames"))))
	(dolist (frame (deski::thread-frames nil))
	  (clim:formatting-row (stream)
	    (clim:formatting-cell (stream :align-x :left :align-y :top)
	      (clim:present frame 'deski::frame
			    :view clim:+textual-view+
			    :stream stream))))))))
    
    
(defun draw-small-frame-window (stream frame)
  (let* ((sc (/ (* 1.9 (clim:stream-string-width stream #\M))
		(clim:rectangle-width
		 (clim:sheet-region (clim:graft stream)))))
	 (scaling (clim:make-scaling-transformation sc sc)))
    (clim:with-room-for-graphics (stream :first-quadrant nil)
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	  (clim:transform-region scaling
				 (clim:sheet-region (clim:graft stream)))
	(clim:draw-rectangle* stream min-x min-y max-x max-y :filled nil))
      (clim:with-bounding-rectangle* (min-x min-y max-x max-y)
	  (clim:transform-region scaling
				 (clim:transform-region (clim:sheet-transformation
							 (clim:frame-top-level-sheet frame))
							(clim:sheet-region
							 (clim:frame-top-level-sheet frame))))
	(clim:draw-rectangle* stream min-x min-y max-x max-y :filled t :ink clim:+grey70+)))))

(defun bytes-to-string (object)
  (if (zerop object)
      "0"
      (let* ((suffixes '(" bytes" " KB" " MB" " GB" " TB" " PB"))
	     (x (floor (realpart (log object 1000))))
	     (idx (min x (1- (length suffixes)))))
	(if (zerop idx)
	    (format nil "~A bytes" object)
	    (format nil "~,1F~A" (/ object (expt 1000 idx)) (nth idx suffixes))))))

(clim:define-command-table menubar-task-command-table
    :menu (("Clear interactor" :command (com-clear-interactor))
	   ("Quit" :command (com-quit))))
