(in-package :desktop-console)

(defclass listener-interactor-pane (clim:interactor-pane) ())

(clim:define-application-frame desktop-console ()
  ((system-debugger))
  (:menu-bar t)
  (:command-table (desktop-console
		   :inherit-from (deski:application-command-table
				  deski:frame-command-table
				  deski:thread-command-table)
		   :menu (("Quit" :command (com-quit))
			  ("Console" :menu menubar-console-command-table)
			  ("Resource" :menu menubar-resource-command-table)
			  ("Tools" :menu menubar-tool-command-table))))
  (:disabled-commands)
  (:panes
   (application-display :application
			:width 200
			:height 300
			:display-function #'%update-application-display
			:display-time t)
   (interactor-container
    (clim:make-clim-stream-pane
     :type 'listener-interactor-pane
     :name 'interactor :scroll-bars t
     ;;:default-view +listener-view+
     :width 650
     :height 300))
   (doc :pointer-documentation)
   (wholine (clim:make-pane 'wholine-pane))
   (debugger-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-debugger-option)
      (clim:radio-box-current-selection "system")
      "swank"
      "clim"
      "desktop")))
  (:top-level (clim:default-frame-top-level :prompt 'print-listener-prompt))
  (:layouts
   (default
       (clim:horizontally ()
	 (clim:vertically ()
           (clim:labelling (:label "Debugger")
             debugger-option)
           (clim:+fill+
            (clim:labelling (:label "Applications")
	      application-display)))
	 (clim:+fill+
	  (clim:labelling (:label "Console")
	    (clim:vertically ()
	      (clim:+fill+ interactor-container)
	      doc
	      wholine)))))))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-console))
  (declare (ignore fm))
  (with-slots (system-debugger) frame
    (setf system-debugger *debugger*))
  (in-package :desk-user))

(defmethod clim:disown-frame  :after (fm (frame desktop-console))
  (declare (ignore fm))
  (with-slots (system-debugger) frame
    (use-debugger system-debugger)))
  
;; utility

(defun print-listener-prompt (stream frame)
  (declare (ignore frame))
  (print-package-name stream)
  (princ "> " stream))

;; output

(defmethod clim:frame-standard-output ((frame desktop-console))
  (clim:get-frame-pane frame 'interactor))

;; read command

(clim:define-presentation-type empty-input ())

(clim:define-presentation-method clim:present 
    (object (type empty-input) stream view &key &allow-other-keys)
  (princ "" stream))

(defmethod clim:read-frame-command ((frame desktop-console) &key (stream *standard-input*))  
  "Specialized for the listener, read a lisp form to eval, or a command."
  (multiple-value-bind (object type)
      (let* ((clim:*command-dispatchers* '(#\,))
	     (command-table (clim:frame-command-table frame))
	     (clim:*accelerator-gestures* (climi::compute-inherited-keystrokes command-table)))
	(handler-case
	    (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
	      (clim:accept 'clim:command-or-form :stream stream :prompt nil 
			   :default "hello" :default-type 'empty-input))
	  (clim:accelerator-gesture (c)
	    (let ((command
		   (clim:lookup-keystroke-command-item (clim:accelerator-gesture-event c)
						       command-table)))
	      (if (and (listp command)
		       (clim:partial-command-p command))
		  (funcall clim:*partial-command-parser*
			   command-table stream command
			   (position clim:*unsupplied-argument-marker* command))
		 (values command 'clim:command))))))
    (cond
      ((clim:presentation-subtypep type 'empty-input)
       ;; Do nothing.
       `(com-eval (values)))
      ((clim:presentation-subtypep type 'command) object)
      (t `(com-eval ,object)))))

;; updating

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

(defun %update-application-display (desktop-console stream)
  (declare (ignore desktop-console))
  (dolist (app (sort (registered-applications) #'string< :key #'application-pretty-name))
    (when (application-menu-p app)
      (fresh-line stream)
      (clim:present app
                    'application
                    :view clim:+textual-view+
                    :stream stream)))
  (fresh-line stream))

;;;
;;; Menu
;;;

(clim:define-command-table menubar-console-command-table
    :menu (("Clear Output" :command (com-clear-output))
	   ("Refresh apps" :command (com-refresh))
	   ("Quit" :command (com-quit))))

(clim:define-command-table menubar-resource-command-table
    :menu (("Thread" :menu deski:thread-command-table)
	   ("Frame" :menu deski:frame-command-table)
	   ("Application" :menu deski:application-command-table)))

(clim:add-command-to-command-table 'deski::com-list-threads 'menubar-resource-command-table
				   :name t
				   :menu t
				   :errorp nil)

(clim:add-command-to-command-table 'deski::com-list-frames 'menubar-resource-command-table
				   :name t
				   :menu t
				   :errorp nil)

(clim:add-command-to-command-table 'deski::com-list-apps 'menubar-resource-command-table
				   :name t
				   :menu t
				   :errorp nil)

(clim:define-command-table menubar-tool-command-table)

(clim:add-menu-item-to-command-table 'menubar-tool-command-table
				     "App Manager"
				     :command `(deski::com-launch-app ,(find-application "app-manager")) :errorp nil)

(clim:add-menu-item-to-command-table 'menubar-tool-command-table
				     "Task Manager"
				     :command `(deski::com-launch-app ,(find-application "task-manager")) :errorp nil)

