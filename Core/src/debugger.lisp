(in-package :desktop-internals)

;;;;
;;;; Debugging
;;;;

(defvar *debugger* nil
  "The current debugger")

;;; functions

(defun use-debugger (debugger)
  (setq *debugger* debugger))

;;; macros

(defmacro with-debugger ((debugger) &body body)
  `(let ((*debugger* ,debugger))
     ,@body))

;;;
;;; debugger hook
;;;

(defun debugger-hook (condition me-or-my-encapsulation)
  (when *debugger*
    (funcall *debugger* condition me-or-my-encapsulation)))
