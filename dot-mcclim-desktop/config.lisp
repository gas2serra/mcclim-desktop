(in-package :desktop-user)

(find-applications)

(configure-application (find-application :listener))
(configure-application (find-application :climacs))
(use-application-as-external-debugger "swank-debugger")
(use-application-as-debugger "desktop-debugger")


