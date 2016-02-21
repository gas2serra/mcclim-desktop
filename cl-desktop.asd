#|
  This file is a part of cl-desktop project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:cl-desktop-asd
  (:use :cl :asdf))
(in-package #:cl-desktop-asd)

(defsystem cl-desktop
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:log4cl)
  :components ((:module "src" :serial t
			:components
			((:file "cl-desktop")))
	       (:module "src/core" :serial t
			:components
			((:file "global")
			 (:file "application")
			 (:file "simple-application-mixin")
			 (:file "manager")
			 (:file "simple-manager-mixin")))
	       (:module "src/standard" :serial t
			:components
			((:file "config")
			 (:file "standard-application-mixin")
			 (:file "standard-application")
			 (:file "standard-manager-mixin"))))
  :description ""
  :long-description ""
  :in-order-to ((test-op (test-op cl-desktop-test))))
