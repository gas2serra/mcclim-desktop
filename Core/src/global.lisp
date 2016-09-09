(in-package :mcclim-desktop-core)

;;;;
;;;; Global Variables
;;;;

(defvar *application* nil
  "The current application")

(defvar *application-style* nil
  "The current application style")

(defvar *manager* nil
  "The manager")

;;; debugger hook
(defun debugger-hook (condition me-or-my-encapsulation)
  (when *manager*
    (funcall (manager-debugger-fn *manager*) condition me-or-my-encapsulation)))

