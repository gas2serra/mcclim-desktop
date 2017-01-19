(in-package :desktop-app-manager)

(clim:define-application-frame desktop-app-manager ()
  (
   (system-style)
   (view-option :initform "menu"))
  (:menu-bar menubar-command-table)
  (:command-table (desktop-app-manager
		   :inherit-from (deski::desktop-application-command-table)))
  (:panes
   (application :application
		:height 300
		:width 300
		:display-time nil)
   (application-display :application
			:height 300
			:width 150
			:display-function #'%update-application-display
			:display-after-commands nil)
   (interactor :interactor :display-time :command-loop)
   (edit-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-edit-option)
      (clim:radio-box-current-selection "yes")
      "no"))
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
      "all")))
  (:layouts
   (default
    (clim:vertically ()
      (2/3
       (clim:horizontally nil
         (clim:labelling (:label "Applications")
           application-display)
	 application
         (clim:vertically nil
           (clim:labelling (:label "View")
             view-option)
           (clim:labelling (:label "Edit local files")
             edit-option)
           (clim:labelling (:label "Style")
             style-option)
	   clim:+fill+)))
      (1/3 (clim:labelling (:label "Interactor")
             interactor))))))

;; output

(defmethod clim:frame-standard-output ((frame desktop-app-manager))
  (clim:find-pane-named clim:*application-frame* 'application))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-app-manager))
  (declare (ignore fm))
  (with-slots (system-style) frame
    (setf system-style *application-style*)))

(defmethod clim:disown-frame  :after (fm (frame desktop-app-manager))
  (declare (ignore fm))
  (setf (logger-stream *logger*) *trace-output*)
  (with-slots (system-style) frame
    (setf *application-style* system-style)))

;; commands

;; updating

(defun %update-edit-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (setf deski::*force-user-app-files-p*
	(string= (clim:gadget-label selected-gadget) "yes")))

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
  (dolist (app (deski::registered-applications))
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
        
(defun %update-application-display (desktop-app-manager stream)
  (declare (ignore desktop-app-manager))
  (with-slots (view-option) clim:*application-frame*
    (dolist (app (deski::registered-applications))
      (if (or (string= view-option "all") (application-menu-p app))
	  (progn
	    (clim:present (find-application app) 'application :stream stream)
	    (format stream "~%"))))))

;;;
;;; Menu
;;;

(clim:make-command-table 'menubar-command-table
			 :errorp nil
			 :menu '(("Quit" :command com-quit)
				 ("Refresh" :command com-refresh)))

