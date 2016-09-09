(in-package :mcclim-desktop-launcher)

(defun run-launcher-gui (&rest args)
  (declare (ignore args))
  (setf *applications* (mcclim-desktop:manager-applications *manager*))
  (clim:run-frame-top-level
   (clim:make-application-frame 'launcher-frame :pretty-name "Launcher")))

(defun run-launcher ()
  (launch-application
   (find-application "launcher")))
