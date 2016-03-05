(in-package :cl-desktop)

(setf (cl-desktop::manager-force-debug-p *manager*) t)
(setf (manager-debugger-fn *manager*) #'clim-debugger:debugger)
(cl-desktop::log-info "cl desktop initialized")

;;(climi::use-pixie)
