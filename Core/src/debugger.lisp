(in-package :desktop-internals)

;;;;
;;;; Debugging
;;;;

(defvar *debugger* nil
  "The current debugger")

;;;
;;; debugger hook
;;;

(defun debugger-hook (condition me-or-my-encapsulation)
  (if (eq (climi::port-event-process (climi::find-port))
	  (climi::current-process))
      (funcall  #'swank:swank-debugger-hook condition me-or-my-encapsulation)
      (when *debugger*
	(funcall *debugger* condition me-or-my-encapsulation))))

(defvar *desktop-debugger-hook* #'debugger-hook)
;;; functions

(defun init-debugger ()
  (setf *debugger* *debugger-hook*)
  (setf *debugger-hook* #'debugger-hook))

(defun use-debugger (debugger)
  (log-info (format nil "Use debugger: ~A~%" debugger))
  (setq *debugger* debugger))

;;; macros

(defmacro with-debugger ((debugger) &body body)
  `(let ((*debugger* ,debugger))
     ,@body))
