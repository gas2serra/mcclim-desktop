#|
  This file is a part of mcclim-desktop project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-desktop-asd
  (:use :cl :asdf))
(in-package #:mcclim-desktop-asd)

(defsystem mcclim-desktop-apps
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim-desktop
	       :mcclim-desktop-launcher
	       :mcclim-desktop-app-manager
	       :mcclim-desktop-task-manager
	       :mcclim-desktop-apropos
	       :mcclim-desktop-debugger
	       :mcclim-desktop-console
	       )
  :components ((:file "packages"))
  :description "McCLIM Desktop Apps")
