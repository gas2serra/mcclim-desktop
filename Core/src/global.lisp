(in-package :mcclim-desktop-core)

;;;;
;;;; Global Variables
;;;;

(defvar *application* nil
  "The current application")

(defvar *application-style* nil
  "The current application style")

(defvar *debugger* nil
  "The current debugger")

;;; debugger hook
(defun debugger-hook (condition me-or-my-encapsulation)
  (when *debugger*
    (funcall *debugger* condition me-or-my-encapsulation)))

