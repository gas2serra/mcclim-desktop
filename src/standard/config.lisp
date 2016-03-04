(in-package :cl-desktop)

;;;;
;;;; Application and Config Files
;;;;

(defparameter *cl-desktop-directory*
  (asdf:component-pathname (asdf:find-system :cl-desktop)))

(defparameter *system-directory* (uiop:merge-pathnames* "etc/" *cl-desktop-directory*))
(defparameter *user-directory* (uiop:merge-pathnames* "~/.cl-desktop/"))

(defparameter *init-file-name* "init.lisp")
(defparameter *application-file-name* "apps/~A.lisp")
(defparameter *application-loading-file-name* "apps/~A-loading.lisp")
(defparameter *application-config-file-name* "apps/~A-config.lisp")

(defparameter *cl-desktop-search-pathnames* 
  (list *user-directory* *system-directory*))

;;;
;;; Utility
;;;

(defun find-file (relative-pathname)
   (find-if #'probe-file
	    (mapcar #'(lambda (d) (uiop:merge-pathnames*
				   relative-pathname d))
		    *cl-desktop-search-pathnames*)))


;;;
;;; Create a new application or config file
;;;

(defun create-user-config-file (name)
  (let ((dest (uiop:merge-pathnames*
		   (format nil *application-config-file-name* name)
		   *user-directory*)))
    (uiop:ensure-all-directories-exist (list dest))
    (uiop:copy-file (uiop:merge-pathnames*
		     (format nil *application-config-file-name* "_%sample_")
		     *system-directory*)
		    dest)))
  
(defun create-user-application-file (name)
  (let ((dest (uiop:merge-pathnames*
		   (format nil *application-file-name* name)
		   *user-directory*)))
    (uiop:ensure-all-directories-exist (list dest))
    (uiop:copy-file (uiop:merge-pathnames*
		     (format nil *application-file-name* "_%sample_")
		     *system-directory*)
		    dest)))
