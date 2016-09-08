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
  :depends-on (:mcclim :mcclim-desktop-core
		       :clouseau :clim-listener :trivial-open-browser
		       :mcclim-desktop-launcher)
  :components ((:file "mcclim-desktop"))
  :description "")
