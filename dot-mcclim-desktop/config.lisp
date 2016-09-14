(in-package :desktop-user)

(use-application-as-debugger "swank-debugger")
(setf *application-style* :my)
(find-applications)




(configure-application (find-application :listener))
(configure-application (find-application :climacs))


(setf clim:*default-server-path*  (list :clx))
#+nil
(progn
  (ql:quickload :mcclim-clxv3/pretty)
  (setf clim:*default-server-path*  (list :clxv3 :host ""
					   :protocol :unix
					   :display-id 0)))

