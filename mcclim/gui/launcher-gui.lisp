(in-package :mcclim-desktop)

(defvar *applications* nil)

(defun edit-file (filename)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename))))


(defun register-launcher-applications (&rest names)
  (dolist (name names)
    (push (find-application name) *applications*)))

(clim:define-application-frame launcher-frame ()
  () 
  (:panes
   (application :application
		:display-function #'display-commands
		:display-after-commands nil)
   (app :application :height 100
	:display-time nil))
  (:layouts
   (defaults
       (clim:vertically ()
	 application app))))

(defun display-commands (launcher-frame stream)
  (declare (ignore launcher-frame))
  (dolist (ae *applications*)
    (clim:present ae 'application :stream stream)))

(clim:define-presentation-method clim:present (app (type application) stream (view clim:textual-view) &key)
  (with-accessors ((label cl-desktop:application-pretty-name)) app
    (format stream "~A~%" label)))

(define-launcher-frame-command com-launch-app ((app 'application))
  (launch-application app))

(define-launcher-frame-command com-edit-config-file ((app 'application))
  (when (application-config-file app)
    (edit-file (application-config-file app))))

(define-launcher-frame-command com-load-application ((app 'application))
  (load-application-system app t))

(define-launcher-frame-command com-configure-application ((app 'application))
  (configure-application app t))


(define-launcher-frame-command com-edit-config-file ((app 'application))
  (when (application-config-file app)
    (edit-file (application-config-file app))))



(define-launcher-frame-command com-edit-application-file ((app 'application))
  (when (application-file app)
    (edit-file (application-file app))))

;;
;; command traslators
;;

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

(clim:define-presentation-to-command-translator edit-application-file
    (application com-edit-application-file launcher-frame
		 :gesture :help
		 :documentation "Edit Application File")
    (object) (list object))

(clim:define-presentation-to-command-translator load-application
    (application com-load-application launcher-frame
		       :gesture :help
		       :documentation "Load Application")
  (object) (list object))

(clim:define-presentation-to-command-translator configure-application
    (application com-configure-application launcher-frame
		 :gesture :help
		 :documentation "Configure Application")
  (object) (list object))


(defmethod clim:frame-standard-output ((frame launcher-frame))
  (clim:get-frame-pane frame 'app))

(defun run-launcher-gui (&rest args)
  (declare (ignore args))
  (clim:run-frame-top-level
   (clim:make-application-frame 'launcher-frame :pretty-name "Launcher")))
