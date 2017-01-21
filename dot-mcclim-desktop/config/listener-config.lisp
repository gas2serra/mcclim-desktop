(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (clim-listener:run-listener)))



;;;
;;; run panter applications
;;;

(clim-listener::define-listener-command (com-run-apropos-navigator :name t)
    nil
  (run-app "apropos-navigator"))

(clim-listener::define-listener-command (com-run-task-manager :name t)
    nil
  (run-app "task-manager"))

;;;
;;; debugger
;;;

(clim-listener::define-listener-command (com-enable-desktop-debugger :name t) ()
  (format t "Enabled desktop debugger~%")
  (use-application-as-debugger "desktop-debugger"))

(clim-listener::define-listener-command (com-enable-swank-debugger :name t) ()
  (format t "Enabled swank debugger~%")
  (use-application-as-debugger "swank-debugger"))
