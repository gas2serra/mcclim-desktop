#|
  This file is a part of cl-desktop project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-desktop-asd
  (:use :cl :asdf))
(in-package #:mcclim-desktop-asd)

(defsystem mcclim-desktop-debugger
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :clouseau :swank)
  :components (;;(:file "mcclim-desktop-debugger")
	       (:module "src"
			:serial t
			;;:depends-on ("mcclim-desktop-debugger")
			:components
			((:file "clim-debugger")
			 (:file "parameters"))))
  :description ""
  :long-description "")
