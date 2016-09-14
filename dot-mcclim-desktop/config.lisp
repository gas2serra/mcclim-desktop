(in-package :desktop-user)

(use-application-as-debugger "swank-debugger")
(setf *application-style* :my)
(find-applications)
(configure-application (find-application :listener))
(configure-application (find-application :climacs))
