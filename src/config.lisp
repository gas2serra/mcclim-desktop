(in-package :mcclim-desktop)

;; config files

(defun default-system-config-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/config/~A-config.lisp" name)
   *mcclim-desktop-directory*))

(defun default-user-config-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/config/~A-config.lisp" name)
   *user-directory*))

(defun default-config-file (name)
  (or
   (probe-file (default-user-config-file name))
   (probe-file (default-system-config-file name))))

;; application files

(defun default-system-application-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/applications/~A.lisp" name)
   *mcclim-desktop-directory*))

(defun default-user-application-file (name)
  (uiop:merge-pathnames*
   (format nil "etc/applications/~A.lisp" name)
   *user-directory*))

(defun default-application-file (name)
  (or
   (probe-file (default-user-application-file name))
   (probe-file (default-system-application-file name))))


;; loading

(defun load-default-system-config-file (name)
  (let ((system-config-file (default-system-config-file name)))
    (when (probe-file system-config-file)
      (load system-config-file))))
			 
(defun load-default-config-file (name)
  (let ((config-file (default-config-file name)))
    (when (and config-file (probe-file config-file))
      (load config-file))))

(defun load-default-application-file (name)
  (let ((application-file (default-application-file name)))
    (when (probe-file application-file)
      (load application-file))))
