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

(defsystem mcclim-desktop-core
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :alexandria)
  :components (;;(:file "mcclim-desktop-core")
	       (:module "src"
			:serial t
			;;:depends-on ("mcclim-desktop-core")
			:components
			((:file "global")
			 (:file "logger")
			 (:file "application")
			 (:file "applications")
			 (:file "standard-pathnames")
			 (:file "application-discovery")
			 (:file "application-mixins")
			 (:file "standard-application")
			 (:file "api")
			 (:file "init"))))
  :description ""
  :long-description "")
