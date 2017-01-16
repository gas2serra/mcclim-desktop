#|
  This file is a part of mcclim-desktop project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Debugger

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-desktop-debugger-asd
  (:use :cl :asdf))
(in-package #:mcclim-desktop-debugger-asd)

(defsystem #:mcclim-desktop-debugger
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :clim-listener :anaphora :clouseau)
  :components ((:module "src"
			:serial t
			:components
			((:file "mcclim-desktop-debugger")
			 (:file "debugger"))))
  :description "Debugger")


