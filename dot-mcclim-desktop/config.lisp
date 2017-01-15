(in-package :desktop-user)

(setf *application-style* :my)
(find-applications)

(configure-application (find-application :listener))
(configure-application (find-application :climacs))
(use-application-as-debugger "panter-debugger")

