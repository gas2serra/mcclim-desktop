(in-package :cl-user)

(defpackage desktop-task-manager
  (:use :desktop :desktop-extensions :cl)
  (:import-from :clim
		)
  (:export
   :run-task-manager
   ))

(in-package :desktop-task-manager)
