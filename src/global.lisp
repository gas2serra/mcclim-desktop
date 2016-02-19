(in-package :mcclim-desktop)

(defparameter *mcclim-directory*
  (asdf:component-pathname (asdf:find-system "mcclim")))

(defparameter *mcclim-desktop-directory*
  (asdf:component-pathname (asdf:find-system :mcclim-desktop)))

(defparameter *user-directory* (uiop:merge-pathnames* "~/.mcclim/desktop"))

(defparameter *debugger-fn* nil)
