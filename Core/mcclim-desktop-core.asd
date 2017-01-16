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

(defsystem mcclim-desktop-core
  :version "0.2"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:bordeaux-threads :alexandria :swank)
  :components ((:module "src"
			:serial t
			:components
			((:file "debugger")
			 (:file "logger")
			 (:file "standard-pathnames")
			 (:file "application")
			 (:file "applications")
			 (:file "application-discovery")
			 (:file "application-mixins")
			 (:file "standard-application")
			 (:file "api")
			 (:file "init"))))
  :description "McCLIM Desktop")

