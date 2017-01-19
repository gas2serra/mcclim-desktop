(in-package :desktop-console)

(clim:define-application-frame desktop-console ()
  ((system-debugger))
  (:menu-bar menubar-command-table)
  (:command-table (desktop-console
		   :inherit-from (deski::desktop-application-command-table
				  deski::frame-command-table
				  deski::thread-command-table)))
  (:panes
   (application-display :application
			:width 150
			:height 300
			:display-function #'%update-application-display
			:display-after-commands nil)
   (interactor :interactor
	       :width 600
	       :height 300
	       :display-time :command-loop)
   (doc :pointer-documentation)
   (wholine (clim:make-pane 'wholine-pane
			    :display-function 'display-wholine :scroll-bars nil
			    :display-time :command-loop :end-of-line-action :allow))
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
	   (clim:+fill+
	    (clim:labelling (:label "Applications")
	      application-display))
	   (clim:labelling (:label "Debugger")
	     debugger-option))
	 (clim:+fill+
	  (clim:labelling (:label "Console")
	    (clim:vertically ()
	      (clim:+fill+ interactor)
	      doc
	      wholine)))))))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-console))
  (declare (ignore fm))
  (with-slots (system-debugger) frame
    (setf system-debugger *debugger*))
  (update-applications))

(defmethod clim:disown-frame  :after (fm (frame desktop-console))
  (declare (ignore fm))
  (setf (logger-stream *logger*) *trace-output*)
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
      (let ((clim:*command-dispatchers* '(#\,)))
        (clim:with-text-style (stream (clim:make-text-style :fix :roman :normal))
          (clim:accept 'clim:command-or-form :stream stream :prompt nil 
		       :default "hello" :default-type 'empty-input)))
    (cond
      ((clim:presentation-subtypep type 'empty-input)
       ;; Do nothing.
       `(com-eval (values)))
      ((clim:presentation-subtypep type 'command) object)
      (t `(com-eval ,object)))))

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

(defun %update-application-display (desktop-console stream)
  (declare (ignore desktop-console))
  (with-slots (view-option) clim:*application-frame*
    (dolist (app *applications*)
      (when (application-menu-p app)
	(clim:present (find-application app) 'application :stream stream)
	(format stream "~%")))))


;;;
;;; Menu
;;;

(clim:make-command-table 'menubar-console-command-table
			 :errorp nil
			 :menu '(("Clear Output" :command (com-clear-output))
				 ("Quit" :command (com-quit))))

(clim:make-command-table 'menubar-application-command-table
			 :errorp nil
			 :menu '(("Refresh" :command (com-refresh))))

(clim:make-command-table 'menubar-command-table
			 :errorp nil
			 :menu '(("Console" :menu menubar-console-command-table)
				 ("Application" :menu menubar-application-command-table)))
				 
				 
(defvar *applications* nil)

(defun update-applications ()
  (setf *applications*
	(sort (applications)
	      #'string<
	      :key #'application-pretty-name)))
