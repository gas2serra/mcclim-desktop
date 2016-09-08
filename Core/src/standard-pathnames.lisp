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

(defun refresh-desktop-search-pathnames ()
  (setf *mcclim-desktop-search-pathnames* (list *system-directory*))
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (when (cdr v)
		 (let ((p (uiop:merge-pathnames* "mcclim-desktop/"
						 (asdf:component-pathname (cdr v)))))
		   (when (probe-file p)
		     (push p *mcclim-desktop-search-pathnames*)))))
	   asdf::*defined-systems*)
  (push *user-directory* *mcclim-desktop-search-pathnames*))

;;;
;;; Utility
;;;

(defun find-file (relative-pathname)
   (find-if #'probe-file
	    (mapcar #'(lambda (d) (uiop:merge-pathnames*
				   relative-pathname d))
		    *mcclim-desktop-search-pathnames*)))

(defun find-user-file (relative-pathname)
   (probe-file (uiop:merge-pathnames*
		relative-pathname *user-directory*)))

;;;
;;; Create a new application or config file
;;;

(defun create-user-config-file (name)
  (let ((dest (uiop:merge-pathnames*
		   (format nil *application-config-file-name* name)
		   *user-directory*))
	(source (or
		 (find-file (format nil *application-config-file-name* name))
		 (find-file (format nil *application-config-file-name* "_%sample_")))))
    (uiop:ensure-all-directories-exist (list dest))
    (uiop:copy-file source dest)
    source))
  
(defun create-user-application-file (name)
  (let ((dest (uiop:merge-pathnames*
		   (format nil *application-file-name* name)
		   *user-directory*))
	(source (or
		 (find-file (format nil *application-file-name* name))
		 (find-file (format nil *application-file-name* "_%sample_")))))
    (uiop:ensure-all-directories-exist (list dest))
    (uiop:copy-file source dest)
    source))

(defun user-config-file (name)
  (or (find-user-file (format nil *application-config-file-name* name))
      (create-user-config-file name)))

(defun user-application-file (name)
  (or (find-user-file (format nil *application-file-name* name))
      (create-user-application-file name)))
