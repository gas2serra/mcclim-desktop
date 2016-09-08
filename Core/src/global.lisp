(in-package :cl-desktop)

;;;;
;;;; Global Variables
;;;;

(defvar *application* nil
  "The current application")

(defvar *manager* nil
  "The manager")

(defparameter *mcclim-directory*
  (asdf:component-pathname (asdf:find-system "mcclim")))