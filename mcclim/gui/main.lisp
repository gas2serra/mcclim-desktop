(in-package :mcclim-desktop)

(defun launcher-run ()
  (initialize-mcclim-manager)
  (launch-application (find-application "launcher")))
