(in-package :desktop-user)

(when *application*
  (setf beirc:*beirc-user-init-file* (application-style-file *application*))
  (setf beirc:*default-fill-column* 110)
  ;;(setf beirc:*default-sound-player* nil)
  (setf beirc:*default-nick* (format nil "mcclim-desktop"))
  (setf beirc::*default-realname* (format nil "Alessandro"))
  (setf beirc:*default-web-browser "/usr/bin/firefox")
  (setf beirc::*auto-connect-list* '("irc.freenode.net"))
  (setf beirc::*auto-identify-list* '("irc.freenode.net"))
  (setf beirc::*nickserv-password-alist* '(("irc.freenode.net" . "password")))
  (setf beirc:*auto-join-alist* '(("irc.freeenode.net" . ("#maxima" "#lisp" "#clnoobs" "#sbcl" "#lispgames" "#scheme" "#emacs" "#algorithms" "##cs" "##programming" "##math" "##asm" "##linux" "##kernel" "##slackware" "#freenode")))))


