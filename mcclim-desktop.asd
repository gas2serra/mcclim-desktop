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

(defsystem mcclim-desktop
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :mcclim-truetype :clouseau :clim-listener :cl-desktop)
  :components ((:module "mcclim" :serial t
			:components
			((:file "mcclim-desktop")
			 (:file "global")
			 (:file "mcclim-debugger")
			 (:file "mcclim-manager")
			 (:file "init")))
	       (:module "mcclim/gui" :serial t
		:components
		((:file "launcher-gui")	
		 (:file "main"))))	
  :description "")
