(in-package :mcclim-desktop-core)

;;; debuggers
(defparameter *clim-debugger* #'clim-debugger:debugger)
(defparameter *swank-debugger* #'swank:swank-debugger-hook)

;;; debugger hook
(defun debugger-hook (condition me-or-my-encapsulation)
  (when *manager*
    (funcall (manager-debugger-fn *manager*) condition me-or-my-encapsulation)))

