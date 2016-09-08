(in-package :mcclim-desktop-core)

;;;;
;;;; Application and Config Files
;;;;

(defparameter *mcclim-desktop-directory*
  (asdf:component-pathname (asdf:find-system :mcclim-desktop)))

(defparameter *system-directory* (uiop:merge-pathnames* "etc/" *mcclim-desktop-directory*))
(defparameter *user-directory* (uiop:merge-pathnames* "~/.mcclim-desktop/"))

(defparameter *init-file-name* "manager-init.lisp")
(defparameter *manager-config-file-name* "manager-config.lisp")
(defparameter *application-file-name* "apps/~A.lisp")
(defparameter *application-config-file-name* "config/~A-config.lisp")

(defparameter *mcclim-desktop-search-pathnames* 
  (list *user-directory* *system-directory*))

;;;
;;; Utility
;;;

(defun find-file (relative-pathname)
   (find-if #'probe-file
	    (mapcar #'(lambda (d) (uiop:merge-pathnames*
				   relative-pathname d))
		    *mcclim-desktop-search-pathnames*)))


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
