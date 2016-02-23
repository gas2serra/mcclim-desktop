(in-package :mcclim-desktop)

(defun launcher-run ()
  (initialize)
  (launch-application (find-application "launcher")))
