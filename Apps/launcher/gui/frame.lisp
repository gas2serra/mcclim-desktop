(in-package :desktop-launcher)

(defclass log-display-pane (clim:application-pane)
  ())

(defmethod clim:note-sheet-grafted ((sheet log-display-pane))
  (setf (logger-stream *logger*) sheet))
  
(defmethod clim:note-sheet-degrafted ((sheet log-display-pane))
  (setf (logger-stream *logger*) *trace-output*))


(clim:define-application-frame desktop-launcher ()
  ((system-debugger)
   (system-style)
   (view-option :initform "menu"))
  (:menu-bar menubar-command-table)
  (:command-table (desktop-launcher
		   :inherit-from (deski::desktop-application-command-table)))
  (:panes
   (application-display :application
			:height 300
			:width 50
			:display-function #'%update-application-display
			:display-after-commands nil)
   (interactor :interactor :display-time :command-loop)
   (log-display-dummy
    (clim:make-clim-stream-pane :name 'log-display :type 'log-display-pane :borders t
				:display-time nil))
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
      "desktop"))
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
   (clear-action :push-button
		 :activate-callback #'(lambda (gadget)
					(declare (ignore gadget))
					(clim:execute-frame-command
					 clim:*application-frame* `(com-clear)))
		 :label "Clear Window"))
  (:layouts
   (defaults
       (clim:vertically ()
	 (2/3
	  (clim:horizontally nil
	    (clim:labelling (:label "Applications")
	      application-display)
	    (clim:vertically nil
              (clim:labelling (:label "View")
                view-option)
	      (clim:labelling (:label "Debugger")
		debugger-option)
              (clim:labelling (:label "Edit local files")
		edit-option)
	      (clim:labelling (:label "Style")
                style-option)
	      clim:+fill+
	      (clim:labelling (:label "Actions")
			      clear-action))))
	 (1/3 (clim:labelling (:label "Log/Output")
		log-display-dummy))))
   (interactor
    (clim:vertically ()
      (2/3
       (clim:horizontally nil
         (clim:labelling (:label "Applications")
           application-display)
         (clim:vertically nil
           (clim:labelling (:label "View")
             view-option)
           (clim:labelling (:label "Debugger")
             debugger-option)
           (clim:labelling (:label "Edit local files")
             edit-option)
           (clim:labelling (:label "Style")
             style-option)
	   clim:+fill+)))
      (1/3 (clim:labelling (:label "Interactor")
             interactor))))))

;; output

(defmethod clim:frame-standard-output ((frame desktop-launcher))
  (clim:find-pane-named clim:*application-frame* 'application-display))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-launcher))
  (declare (ignore fm))
  (with-slots (system-debugger system-style) frame
    (setf system-debugger *debugger*)
    (setf system-style *application-style*))
  (update-applications))

(defmethod clim:disown-frame  :after (fm (frame desktop-launcher))
  (declare (ignore fm))
  (setf (logger-stream *logger*) *trace-output*)
  (with-slots (system-debugger system-style) frame
    (use-debugger system-debugger)
    (setf *application-style* system-style)))

;; commands

(define-desktop-launcher-command (com-set-layout :name nil :menu nil)
    ((layout-name 'symbol))
  (with-accessors ((layout clim:frame-current-layout)) clim:*application-frame*
    (setf layout layout-name)))

;; updating

(defun %update-edit-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (setf deski::*force-user-app-files-p*
	(string= (clim:gadget-label selected-gadget) "yes")))

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
	((string= label "desktop")
	 (use-application-as-debugger "desktop-debugger"))))))

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
	  (progn
	    (clim:present (find-application app) 'application :stream stream)
	    (format stream "~%"))))))

;;;
;;; Menu
;;;

(clim:make-command-table 'layout-command-table
			 :errorp nil
			 :menu '(("Log/Output" :command (com-set-layout defaults))
				 ("Interactor" :command (com-set-layout interactor))))

(clim:make-command-table 'menubar-command-table
			 :errorp nil
			 :menu '(("Quit" :command com-quit)
				 ("Refresh" :command com-refresh)
				 ("Layout" :menu layout-command-table)))

