(in-package :desktop-internals)

;;;;
;;;; Debugging
;;;;

(defvar *debugger* nil
  "The current debugger")

;;;
;;; debugger hook
;;;


(defun desktop-swank-debugger-hook (condition me-or-my-encapsulation)
  (unless swank::*connections*
    (launch-app :swank-server)
    (sleep 3.0)
    (launch-app :slime)
    (sleep 5.0))
  (funcall #'swank:swank-debugger-hook condition me-or-my-encapsulation))

(defun global-debugger-hook (condition me-or-my-encapsulation)
  (format *debug-io* "GLOBAL DEB~%")
  (swank/backend:call-with-debugger-hook
   #'desktop-swank-debugger-hook
   (lambda ()
     (if (or (eq (climi::port-event-process (climi::find-port))
		 (climi::current-process))
	     (null *debugger*))
	 (funcall  #'desktop-swank-debugger-hook condition me-or-my-encapsulation)
	 (funcall *debugger* condition me-or-my-encapsulation)))))

(defun debugger-hook (condition me-or-my-encapsulation)
  (format *debug-io* "LOCAL DEB~%")
  (swank/backend:call-with-debugger-hook
   #'global-debugger-hook
   (lambda ()
     (if (or (eq (climi::port-event-process (climi::find-port))
		 (climi::current-process))
	     (null *debugger*))
	 (funcall  #'desktop-swank-debugger-hook condition me-or-my-encapsulation)
	 (funcall *debugger* condition me-or-my-encapsulation)))))

;;; global variables

(defvar *desktop-debugger-hook* #'debugger-hook)
(defvar *desktop-global-debugger-hook* #'global-debugger-hook)

;;; functions

(defun init-debugger ()
  (swank/backend:install-debugger-globally *desktop-global-debugger-hook*))

(defun use-debugger (debugger)
  (log-info (format nil "Use debugger: ~A~%" debugger))
  (setq *debugger* debugger))
