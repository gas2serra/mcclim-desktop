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

(defsystem mcclim-desktop-launcher
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim-desktop)
  :components ((:file "mcclim-desktop-launcher")
	       (:module "gui"
			:serial t
			:depends-on ("mcclim-desktop-launcher")
			:components
			((:file "parameters")
			 (:file "utility")
			 (:file "presentations")
			 (:file "frame")
			 (:file "commands")
			 (:file "main"))))
  :description "")
