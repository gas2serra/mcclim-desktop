#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
 Task Manager

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-desktop-task-manager-asd
  (:use :cl :asdf))
(in-package #:mcclim-desktop-task-manager-asd)

(defsystem #:mcclim-desktop-task-manager
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :anaphora :clouseau :trivial-timers)
  :components ((:file "mcclim-desktop-task-manager")
  	       (:module "gui"
			:serial t
			:components
			((:file "frame")
			 (:file "commands")
			 (:file "main"))))
  :description "Task Manager")
