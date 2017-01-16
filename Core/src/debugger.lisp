(in-package :desktop-internals)

;;;;
;;;; Debugging
;;;;

(defvar *debugger* nil
  "The current debugger")

(defvar *external-debugger* nil
  "The current external debugger")

;;;
;;; debugger hook
;;;

(defun debugger-hook (condition me-or-my-encapsulation)
  (swank/backend:call-with-debugger-hook
   *external-debugger*
   (lambda ()
     (if (or (eq (climi::port-event-process (climi::find-port))
		 (climi::current-process))
	     (null *debugger*))
	 (funcall *external-debugger* condition me-or-my-encapsulation)
	 (funcall *debugger* condition me-or-my-encapsulation)))))

;;; functions

(defun install-debugger-globally ()
  (swank/backend:install-debugger-globally #'debugger-hook))

(defun use-debugger (debugger)
  (log-info (format nil "Use debugger: ~A~%" debugger))
  (setq *debugger* debugger))

(defun use-external-debugger (debugger)
  (log-info (format nil "Use external debugger: ~A~%" debugger))
  (setq *external-debugger* debugger))
