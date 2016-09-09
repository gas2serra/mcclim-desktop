(in-package :mcclim-desktop-core)

;;;;
;;;; Global Variables
;;;;

(defvar *application* nil
  "The current application")

(defvar *manager* nil
  "The manager")

;;;
;;; Debugging
;;;

(defparameter *clim-debugger* #'clim-debugger:debugger)
(defparameter *swank-debugger* #'swank:swank-debugger-hook)

;;; debugger hook
(defun debugger-hook (condition me-or-my-encapsulation)
  (when *manager*
    (funcall (manager-debugger-fn *manager*) condition me-or-my-encapsulation)))

