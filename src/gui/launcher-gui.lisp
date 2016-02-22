(in-package :mcclim-desktop)


(defvar *applications* nil)
(defun edit-file (filename)
  (let ((editor (find-application "editor")))
    (launch-application editor :args (list filename))))


(defclass application-entry ()
  ((name :initarg :name)
   (label :initarg :label)
   (application :initform nil)))

(defun register-launcher-application (name label)
  (push (make-instance 'application-entry :name name :label label) *applications*))

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
  (declare (ignore launcher-frame))
  (dolist (ae *applications*)
    (clim:present ae 'application-entry :stream stream)))

(clim:define-presentation-method clim:present (app (type application-entry) stream (view clim:textual-view) &key)
  (with-slots (label) app
    (format stream "~A~%" label)))

(define-launcher-frame-command com-launch-app ((app 'application-entry))
  (with-slots (application name) app
    (unless application
      (setf application (find-application name)))
    (launch-application application)))

(define-launcher-frame-command com-edit-config-file ((app 'application-entry))
  (with-slots (application name) app
    (unless application
      (setf application (find-application name)))
    (when (application-config-file application)
      (edit-file (application-config-file application)))))

(define-launcher-frame-command com-edit-application-file ((app 'application-entry))
  (with-slots (application name) app
    (unless application
      (setf application (find-application name)))
    (when (application-file application)
      (edit-file (application-file application)))))
     

(clim:define-presentation-to-command-translator launch-app
    (application-entry com-launch-app launcher-frame
		 :gesture :select
		 :documentation "Launch application")
    (object) (list object))


(clim:define-presentation-to-command-translator edit-config-file
    (application-entry com-edit-config-file launcher-frame
		 :gesture :help
		 :documentation "Edit Config File")
    (object) (list object))

(clim:define-presentation-to-command-translator edit-application-file
    (application-entry com-edit-application-file launcher-frame
		 :gesture :help
		 :documentation "Edit Application File")
    (object) (list object))


(defmethod clim:frame-standard-output ((frame launcher-frame))
  (clim:get-frame-pane frame 'app))

(defun run-launcher-gui (&rest args)
  (declare (ignore args))
  (clim:run-frame-top-level
   (clim:make-application-frame 'launcher-frame :pretty-name "Launcher")))
