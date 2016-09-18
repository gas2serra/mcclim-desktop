(in-package :desktop-user)


(setf *application-style* :my)
(find-applications)

(configure-application (find-application :listener))
(configure-application (find-application :climacs))
(use-application-as-debugger "panter-debugger")

(setf clim:*default-server-path*  (list :clx))
#+nil
(progn
  (ql:quickload :mcclim-clxv3/pretty)
  (setf clim:*default-server-path*  (list :clxv3 :host ""
					   :protocol :unix
					   :display-id 0)))

