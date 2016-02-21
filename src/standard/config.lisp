(in-package :cl-desktop)



;;;
;;;
;;;

(defparameter *mcclim-directory*
  (asdf:component-pathname (asdf:find-system "mcclim")))

(defparameter *cl-desktop-directory*
  (asdf:component-pathname (asdf:find-system :cl-desktop)))

(defparameter *user-directory* (uiop:merge-pathnames* "~/.cl-desktop/"))

(defun default-system-config-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/config/~A-config.lisp" name)
   *cl-desktop-directory*))

(defun default-user-config-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/config/~A-config.lisp" name)
   *user-directory*))

(defun default-config-file (name)
  (or
   (probe-file (default-user-config-file name))
   (probe-file (default-system-config-file name))))

(defun load-default-application-file (name)
  (let ((application-file (default-application-file name)))
    (when (probe-file application-file)
      (load application-file))))



;; application files

(defun default-system-application-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/apps/~A.lisp" name)
   *cl-desktop-directory*))

(defun default-user-application-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/apps/~A.lisp" name)
   *user-directory*))

(defun default-application-file (name)
  (or
   (probe-file (default-user-application-file name))
   (probe-file (default-system-application-file name))))



;;;
;;;
;;;

(defgeneric application-file (application))
(defmethod application-file ((application application))
  nil)

;;(defgeneric find-application (application name)

