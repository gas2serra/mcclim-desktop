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
  :components ((:module "src/mcclim" :serial t
                :components
                ((:file "packages")
		 (:file "global")
		 (:file "mcclim-debugger")
		 (:file "mcclim-manager")))
	       (:module "src/gui" :serial t
		:components
		((:file "launcher-gui")	
		 (:file "main"))))	
  :description "")
