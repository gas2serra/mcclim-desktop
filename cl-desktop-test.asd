#|
  This file is a part of cl-desktop project.
  Copyright (c) 2016 Alessandro Serra (gas2serra@gmail.com)
|#

(in-package :cl-user)
(defpackage #:cl-desktop-test-asd
  (:use :cl :asdf))
(in-package #:cl-desktop-test-asd)

(defsystem cl-desktop-test
  :author "Alessandro Serra"
  :license "GPLv3"
  :depends-on (:cl-desktop
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-desktop"))))
  :description "Test system for cl-desktop"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
