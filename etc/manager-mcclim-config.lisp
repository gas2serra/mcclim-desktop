(in-package :cl-desktop-user)

(setf (manager-debugger-fn *manager*)
      #'clim-debugger:debugger
      ;;nil
      ;;#'swank:swank-debugger-hook
      )

