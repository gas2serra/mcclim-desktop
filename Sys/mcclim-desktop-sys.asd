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

(defsystem mcclim-desktop-sys
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:asdf :drei-mcclim)
  :components ((:module "src"
			:serial t
			:components
			((:file "clipboard")
			 (:file "screenshot")
			 (:file "kill-ring"))))
  :description "McCLIM Desktop")
