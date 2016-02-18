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

(define-launcher-frame-command com-edit-system-config
    ((application 'application))
  (with-slots (name) application
    (let ((system-config-file (uiop:merge-pathnames*
			       (format nil "etc/~A.lisp" name)
			       *mcclim-desktop-directory*)))
    (edit-file system-config-file))))

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app launcher-frame
		 :gesture :select
		 :documentation "Launch application")
    (object) (list object))

(clim:define-presentation-to-command-translator edit-system-config
    (application com-edit-system-config launcher-frame
		 :gesture :help
		 :documentation "Edit system config")
    (object) (list object))

(defmethod clim:frame-standard-output ((frame launcher-frame))
  (clim:get-frame-pane frame 'app))

(defun run-launcher-gui ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'launcher-frame :pretty-name "Launcher")))
