(in-package :mcclim-desktop-core)

;;;;
;;;; Application and Config Files
;;;;

(defparameter *system-directory* (uiop:merge-pathnames*
				  "etc/"
				  (asdf:component-pathname (asdf:find-system :mcclim-desktop))))
(defparameter *user-directory* (uiop:merge-pathnames* "~/.mcclim-desktop/"))

(defparameter *init-file-name* "manager-init.lisp")
(defparameter *manager-config-file-name* "manager-config.lisp")
(defparameter *application-file-name* "apps/~A.lisp")
(defparameter *application-config-file-name* "config/~A-config.lisp")
(defparameter *application-style-file-name* "config/~A-~A.lisp")
(defparameter *sample-file-name* "_%sample_")

(defparameter *mcclim-desktop-search-pathnames* 
  (list *user-directory* *system-directory*))

;;;
;;; Relative pathnames
;;;

(defun application-relative-file-pathname (application-name)
  (format nil *application-file-name* (string-downcase application-name)))

(defun application-relative-config-file-pathname (application-name)
  (format nil *application-config-file-name* (string-downcase application-name)))

(defun application-relative-style-file-pathname (application-name style)
  (format nil *application-style-file-name* (string-downcase application-name) (string-downcase style)))

;;;
;;; Find files
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

(defun create-user-file (name sample-name)
  (log-warn "Create file (~A) in the user directory (~A)" name *user-directory*)
  (let ((dest (uiop:merge-pathnames* name *user-directory*))
	(source (or
		 (find-file name)
		 (find-file sample-name))))
    (uiop:ensure-all-directories-exist (list dest))
    (when source
      (uiop:copy-file source dest)
    dest)))
  
;;;
;;; find application's files
;;;

(defun find-application-file (name &optional force-p force-user-p)
  (let ((file 
	 (if force-user-p
	     (find-user-file (application-relative-file-pathname name))
	     (find-file (application-relative-file-pathname name)))))
    (when (and (not file) force-p)
      (setf file (create-user-file
		  (application-relative-file-pathname name)
		  (application-relative-file-pathname *sample-file-name*))))
    file))

(defun find-application-config-file (name &optional force-p force-user-p)
  (let ((file 
	 (if force-user-p
	     (find-user-file (application-relative-config-file-pathname name))
	     (find-file (application-relative-config-file-pathname name)))))
    (when (and (not file) force-p)
      (setf file (create-user-file
		  (application-relative-config-file-pathname name)
		  (application-relative-config-file-pathname *sample-file-name*))))
    file))

(defun find-application-style-file (name style &optional force-p force-user-p)
  (let ((file 
	 (if force-user-p
	     (find-user-file (application-relative-style-file-pathname name style))
	     (find-file (application-relative-style-file-pathname name style)))))
    (when (and (not file) force-p)
      (setf file (create-user-file
		  (application-relative-style-file-pathname name style)
		  (application-relative-style-file-pathname *sample-file-name* "default"))))
    file))

;;;
;;; refresh
;;;

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
