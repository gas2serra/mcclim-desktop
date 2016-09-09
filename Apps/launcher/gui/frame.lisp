(in-package :mcclim-desktop-launcher)

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

(defmethod clim:adopt-frame  :after (fm (frame launcher-frame))
  (declare (ignore fm))
  (setf (manager-log-stream *manager*)
	(clim:get-frame-pane frame 'app)))

(defmethod clim:disown-frame  :after (fm (frame launcher-frame))
  (declare (ignore fm))
  (setf (manager-log-stream *manager*) *trace-output*))

(defun display-commands (launcher-frame stream)
  (declare (ignore launcher-frame))
  (dolist (ae *applications*)
    (clim:present (find-application ae) 'application :stream stream)))




(defmethod clim:frame-standard-output ((frame launcher-frame))
  (clim:get-frame-pane frame 'app))


