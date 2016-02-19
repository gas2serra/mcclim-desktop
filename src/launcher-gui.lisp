(in-package :mcclim-desktop)

(clim:define-application-frame launcher-frame ()
  () 
  (:panes
   (application :application
		:display-function #'display-commands
		:display-after-commands nil)
   (app :application :height 100))
  (:layouts
   (defaults
       (clim:vertically ()
	 application app))))

(defun display-commands (launcher-frame stream) 
  (loop for app being the hash-values of *applications*
     do (clim:present app 'application :stream stream)))

(clim:define-presentation-method clim:present
    (app (type application) stream (view clim:textual-view) &key)
  (format stream "~A~%" (application-pretty-name app)))

(define-launcher-frame-command com-launch-app
    ((application 'application))
  (run-application application))

(define-launcher-frame-command com-edit-config-file
    ((application 'application))
  (with-slots (name config-file) application
    (edit-file (or (and config-file (probe-file config-file))
		   (probe-file (default-user-config-file name))
		   (probe-file (default-system-config-file name))))))

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app launcher-frame
		 :gesture :select
		 :documentation "Launch application")
    (object) (list object))

(clim:define-presentation-to-command-translator edit-config-file
    (application com-edit-config-file launcher-frame
		 :gesture :help
		 :documentation "Edit Config File")
    (object) (list object))

(defmethod clim:frame-standard-output ((frame launcher-frame))
  (clim:get-frame-pane frame 'app))

(defun run-launcher-gui (&rest args)
  (clim:run-frame-top-level
   (clim:make-application-frame 'launcher-frame :pretty-name "Launcher")))
