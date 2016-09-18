(in-package :desktop-internals)

;;;;
;;;; Debugging
;;;;

#|
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
|#

(defvar *desktop-debugger-hook* nil)
(defgeneric use-debugger (debugger))

(defmethod use-debugger (debugger)
  (log-info (format nil "Use debugger: ~A~%" debugger))
  (setf *desktop-debugger-hook* debugger)
  (setf *debugger-hook* debugger))

