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
			((:file "cl-desktop")
			 (:file "global")
			 (:file "application")
			 (:file "manager")
			 (:file "standard-pathnames")
			 (:file "application-mixins")
			 (:file "manager-mixins")
			 (:file "standard-application"))))
  :description ""
  :long-description ""
  :in-order-to ((test-op (test-op cl-desktop-test))))
