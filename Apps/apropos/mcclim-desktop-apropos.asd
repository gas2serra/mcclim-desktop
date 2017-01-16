#|
  This file is a part of mcclim-panter project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

#|
 Apropos Navigator

  Author: Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:mcclim-desktop-apropos-asd
  (:use :cl :asdf))
(in-package #:mcclim-desktop-apropos-asd)

(defsystem #:mcclim-desktop-apropos
  :version "0.1"
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:mcclim :climacs :clim-listener :cl-ppcre :anaphora :swank :closer-mop :clouseau)
  :components ((:file "mcclim-desktop-apropos")
	       (:module "src"
			:serial t
			:depends-on ("mcclim-desktop-apropos")
			:components
			((:file "utility")
			 (:file "iapropos")
			 (:file "iapropos-preselects")
			 (:file "presentations")))
	       (:module "gui"
			:serial t
			:depends-on ("src")
			:components
			((:file "parameters")
			 (:file "frame")
			 (:file "commands")
			 (:file "main"))))
  :description "Apropos Navigator")
