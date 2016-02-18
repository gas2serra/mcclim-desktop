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
  :depends-on (:mcclim :mcclim-truetype :clouseau :clim-listener)
  :components ((:module "src" :serial t
                :components
                ((:file "packages")
		 (:file "configuration")
		 (:file "application")
		 (:file "manager")
		 (:file "launcher-gui")
		 (:file "mcclim-debugger")
		 (:file "main"))))
  :description "")
