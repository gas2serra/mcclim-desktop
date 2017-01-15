(in-package :desktop-launcher)

(clim:define-application-frame desktop-launcher ()
  ((force-user-p :initform t)
   (system-debugger)
   (system-style)
   (view-option :initform "menu"))
  (:menu-bar t)
  (:panes
   (application-display :application
			:height 300
			:width 50
			:display-function #'%update-application-display
			:display-after-commands nil)
   (log-display :application
		:display-time nil)
   (edit-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-edit-option)
      (clim:radio-box-current-selection "yes")
      "no"))
   (debugger-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-debugger-option)
      (clim:radio-box-current-selection "system")
      "swank"
      "clim"
      "panter"))
   (style-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-style-option)
      (clim:radio-box-current-selection "system")
      "my"
      "default"))
   (view-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-view-option)
      (clim:radio-box-current-selection "menu")
      "all"))
   (clear-log-action :push-button
		     :activate-callback #'(lambda (gadget)
					    (declare (ignore gadget))
					    (com-clear-log))
		     :label "Clear Log"))
  (:layouts
   (defaults
       (clim:vertically ()
	 (2/3
	  (clim:horizontally nil
	    (clim:labelling (:label "Applications")
	      application-display)
	    (clim:vertically nil
	      (clim:labelling (:label "Edit local files")
		edit-option)
	      (clim:labelling (:label "Debugger")
		debugger-option)
	      (clim:labelling (:label "Style")
		style-option)
	      (clim:labelling (:label "View")
		view-option)
	      clim:+fill+
	      (clim:labelling (:label "Actions")
		clear-log-action))))
	 (1/3 (clim:labelling (:label "Log/Output")
		log-display))))))

;; output

(defmethod clim:frame-standard-output ((frame desktop-launcher))
  (clim:get-frame-pane frame 'log-display))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-launcher))
  (declare (ignore fm))
  (with-slots (system-debugger system-style) frame
    (setf system-debugger *desktop-debugger-hook*)
    (setf system-style *application-style*))
  (setf (logger-stream *logger*)
	(clim:get-frame-pane frame 'log-display))
  (update-applications))

(defmethod clim:disown-frame  :after (fm (frame desktop-launcher))
  (declare (ignore fm))
  (setf (logger-stream *logger*) *trace-output*)
  (with-slots (system-debugger system-style) frame
    (use-debugger system-debugger)
    (setf *application-style* system-style)))


;; updating

(defun %update-edit-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (force-user-p) clim:*application-frame*
    (setf force-user-p
	  (string= (clim:gadget-label selected-gadget) "yes"))))

(defun %update-debugger-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (system-debugger) clim:*application-frame*
    (let ((label (clim:gadget-label selected-gadget)))
      (cond
	((string= label "system")
	 (use-debugger system-debugger))
	((string= label "swank")
	 (use-application-as-debugger "swank-debugger"))
	((string= label "clim")
	 (use-application-as-debugger "clim-debugger"))
	((string= label "panter")
	 (use-application-as-debugger "panter-debugger"))))))

(defun %update-style-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (system-style) clim:*application-frame*
    (let ((label (clim:gadget-label selected-gadget)))
      (cond
	((string= label "system")
	 (setf *application-style* system-style))
	((string= label "my")
	 (setf *application-style* :my))
	((string= label "default")
	 (setf *application-style* :default)))))
  (dolist (app *applications*)
    (need-reconfigure-application app)))

(defun %update-view-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (view-option) clim:*application-frame*
    (let ((label (clim:gadget-label selected-gadget)))
      (cond
	((string= label "menu")
	 (setf view-option "menu"))
	((string= label "all")
	 (setf view-option "all")))))
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))
  
      
(defun %update-application-display (desktop-launcher stream)
  (declare (ignore desktop-launcher))
  (with-slots (view-option) clim:*application-frame*
    (dolist (app *applications*)
      (if (or (string= view-option "all") (application-menu-p app))
	  (clim:present (find-application app) 'application :stream stream)))))
