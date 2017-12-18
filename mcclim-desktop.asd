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

(defsystem mcclim-desktop/internals
    :components ((:file "packages")))

(defsystem mcclim-desktop
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim
	       :mcclim-desktop/internals
	       :mcclim-desktop-sys
	       :mcclim-desktop-core
	       :mcclim-desktop-resource)
  :components ((:file "init"))
  :description "McCLIM Desktop")

