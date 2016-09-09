(in-package :mcclim-desktop-core)

;;;;
;;;; Application and Config Files
;;;;

(defparameter *system-directory* "dot-mcclim-desktop/")
(defparameter *user-directory* (uiop:merge-pathnames* "~/.mcclim-desktop/"))
(defparameter *application-file-directory* "apps/")
(defparameter *application-config-directory* "config/")

(defparameter *init-file-name* "init.lisp")
(defparameter *manager-config-file-name* "config.lisp")

(defparameter *application-file-name* "~A.lisp")
(defparameter *application-config-file-name* "~A-config.lisp")
(defparameter *application-style-file-name* "~A-~A.lisp")
(defparameter *sample-file-name* "_%sample_")

(defparameter *mcclim-desktop-search-pathnames* nil)

;;;
;;; Relative pathnames
;;;

(defun application-relative-file-pathname (application-name)
  (uiop:merge-pathnames*
   (format nil *application-file-name* (string-downcase application-name))
   *application-file-directory*))

(defun application-relative-config-file-pathname (application-name)
  (uiop:merge-pathnames*
   (format nil *application-config-file-name* (string-downcase application-name))
   *application-config-directory*))

(defun application-relative-style-file-pathname (application-name style)
  (uiop:merge-pathnames*
   (format nil *application-style-file-name* (string-downcase application-name) (string-downcase style))
   *application-config-directory*))

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

(defun create-user-file (name sample-pathname)
  (log-warn (format nil "Create file (~A) in the user directory (~A)" name *user-directory*))
  (let ((dest (uiop:merge-pathnames* name *user-directory*))
	(source (or
		 (find-file name)
		 sample-pathname)))
    (uiop:ensure-all-directories-exist (list dest))
    (when source
      (uiop:copy-file source dest))
    dest))
  
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
		  (or
		   (find-application-file name)
		   (find-application-file *sample-file-name*)))))
    file))

(defun find-application-config-file (name &optional force-p force-user-p)
  (let ((file 
	 (if force-user-p
	     (find-user-file (application-relative-config-file-pathname name))
	     (find-file (application-relative-config-file-pathname name)))))
    (when (and (not file) force-p)
      (setf file (create-user-file
		  (application-relative-config-file-pathname name)
		  (or (find-application-config-file name)
		      (find-application-config-file *sample-file-name*)))))
    file))

(defun find-application-style-file (name style &optional force-p force-user-p)
  (let ((file 
	 (if force-user-p
	     (find-user-file (application-relative-style-file-pathname name style))
	     (find-file (application-relative-style-file-pathname name style)))))
    (when (and (not file) force-p)
      (setf file (create-user-file
		  (application-relative-style-file-pathname name style)
		  (or (find-application-style-file name style)
		      (find-application-style-file name :default)
		      (find-application-style-file *sample-file-name* :default)))))

    file))

;;;
;;; refresh
;;;

(defun refresh-desktop-search-pathnames ()
  (setf *mcclim-desktop-search-pathnames* nil)
  (dolist (system (asdf/find-system:registered-systems*))
    (let ((pathname (asdf:component-pathname system)))
      (when pathname
	(let ((p (uiop:merge-pathnames* *system-directory*
					pathname)))
	  (when (probe-file p)
	    (push p *mcclim-desktop-search-pathnames*))))))
  (push *user-directory* *mcclim-desktop-search-pathnames*))

(defun find-all-application-names ()
  (let ((out))
    (dolist (pathname *mcclim-desktop-search-pathnames*)
      (let ((apps-pathname (uiop:merge-pathnames* *application-file-directory*
						  pathname)))
	(dolist (file (uiop/filesystem:directory-files apps-pathname))
	  (let ((app-name (pathname-name file)))
	    (when (string/= app-name *sample-file-name*)
	      (push app-name out))))))
    out))
