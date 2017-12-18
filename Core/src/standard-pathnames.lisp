(in-package :desktop-internals)


;;;;
;;;; Desktop standard pathnames
;;;;

;;;
;;; user directory
;;;

(defparameter *user-directory* (uiop:merge-pathnames* "~/.mcclim-desktop/"))

(defun find-user-file (relative-pathname &optional error-p)
  (let ((pathname (uiop:merge-pathnames*
		   relative-pathname *user-directory*)))
    (or (probe-file pathname)
	(and error-p (error "file (~A) not found" pathname)))))

(defun create-user-file (relative-pathname &optional copy-from-pathname)
  (log-warn (format nil "Create file (~A) in the user directory (~A)"
		    relative-pathname *user-directory*))
  (let ((dest (uiop:merge-pathnames* relative-pathname *user-directory*))
	(source (or
		 (find-file relative-pathname)
		 copy-from-pathname)))
    (uiop:ensure-all-directories-exist (list dest))
    (when source
      (uiop:copy-file source dest))
    dest))

(defun find-user-files (relative-directory-pathname &optional error-p)
  (let ((pathname (uiop:merge-pathnames* relative-directory-pathname
					 *user-directory*)))
    (when (and (not (probe-file pathname)) error-p)
      (error "file (~A) not found" pathname))
    (uiop/filesystem:directory-files pathname)))

;;;
;;; system directories
;;;

(defvar *system-directories* nil)
(defparameter *system-directory-relative-pathaname* #p"dot-mcclim-desktop/")

(defun find-system-file (relative-pathname &optional error-p)
  (or (find-if #'probe-file
	       (mapcar #'(lambda (d) (uiop:merge-pathnames*
				      relative-pathname d))
		       *system-directories*))
      (and error-p (error "file (~A) not found" relative-pathname))))

(defun find-system-files (relative-pathname)
  (let ((out))
    (dolist (dir *system-directories*)
      (let ((pathname (uiop:merge-pathnames* relative-pathname
					     dir)))
	(when (probe-file pathname)
	  (setf out (append (uiop/filesystem:directory-files pathname) out)))))
    out))

(defun find-system-directories ()
  (setf *system-directories* nil)
  (dolist (system-name (asdf/find-system::registered-systems))
    (let ((pathname (asdf:component-pathname (asdf:find-system system-name))))
      (when pathname
	(let ((p (uiop:merge-pathnames* *system-directory-relative-pathaname*
					pathname)))
	  (when (probe-file p)
	    (push p *system-directories*))))))
  *system-directories*)

;;;
;;; both
;;;

(defun find-file (relative-pathname &optional error-p)
  (or (find-user-file relative-pathname)
      (find-system-file relative-pathname error-p)))

;;;
;;; application files
;;;


(defparameter *application-directory-relative-pathname* "apps/")
(defparameter *application-file-template-name* "~A.lisp")
(defparameter *sample-file-name* "_%sample_")

(defun application-file-relative-pathname (name)
  (uiop:merge-pathnames* (format nil *application-file-template-name*
				 (string-downcase name))
			 *application-directory-relative-pathname*))

(defun find-application-file (name &optional force-p force-user-p)
  (let ((rel-pathname (application-file-relative-pathname name)))
    (let ((file 
	   (or (find-user-file rel-pathname)
	       (and (not force-user-p)
		    (find-system-file rel-pathname)))))
      (when (and (not file) force-p)
	(setf file (create-user-file
		    rel-pathname
		    (or
		     (find-system-file rel-pathname)
		     (find-application-file *sample-file-name*)))))
      file)))

(defun find-application-files ()
  (remove-if #'(lambda (file)
		 (string= (pathname-name file) *sample-file-name*))
	     (append 
	      (find-user-files *application-directory-relative-pathname*)
	      (find-system-files *application-directory-relative-pathname*))))

;;;
;;; config files
;;;

(defparameter *config-directory-relative-pathname* "config/")
(defparameter *config-file-template-name* "~A-~A.lisp")

(defun config-file-relative-pathname (name &optional style)
  (uiop:merge-pathnames* (format nil *config-file-template-name*
				 (string-downcase name)
				 (string-downcase (or style "config")))
			 *config-directory-relative-pathname*))

(defun find-config-file (name &optional style force-p force-user-p)
  (let ((rel-pathname (config-file-relative-pathname name style)))
    (let ((file 
	   (or (find-user-file rel-pathname)
	       (and (not force-user-p)
		    (find-system-file rel-pathname)))))
      (when (and (not file) force-p)
	(setf file (create-user-file
		    rel-pathname
		    (or
		     (find-system-file rel-pathname)
		     (and style
			  (not (eq style :default))
			  (find-config-file name :default nil nil))
		     (find-config-file *sample-file-name* (and style :default))))))
      file)))

;;;
;;; 
;;;

(defparameter *init-file-name* "init.lisp")
(defparameter *config-file-name* "config.lisp")

(defun ensure-all-user-directories-exist ()
  (uiop:ensure-all-directories-exist
   (list *user-directory*
	 (uiop:merge-pathnames*
	  *application-directory-relative-pathname*
	  *user-directory*)
	 (uiop:merge-pathnames*
	  *config-directory-relative-pathname*
	  *user-directory*))))
