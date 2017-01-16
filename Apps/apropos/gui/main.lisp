(in-package :desktop-apropos)

;;;
;;; run
;;;

#|
(defun run-apropos-navigator ()
  (let ((*return-values* nil))
    (let* ((frame (clim:make-application-frame 'apropos-navigator)))
      (setf (clim:frame-current-layout frame) :default)
      (clim:run-frame-top-level frame :name "apropos-navigator"))
    *return-values*))
|#

(defun run-apropos-navigator (&key (new-process nil)
				(width 790)
				(height 550)
				port
				frame-manager
				(pretty-name "Apropos Navigator")
				(process-name "apropos-navigator"))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
         (frame (clim:make-application-frame 'apropos-navigator
					     :pretty-name pretty-name
					     :frame-manager fm
					     :width width
					     :height height)))
    (flet ((run ()
	     (let ((*return-values* nil))	     
	       (unwind-protect (clim:run-frame-top-level frame)
		 (clim:disown-frame fm frame))
	       *return-values*)))
      (if new-process
          (values (clim-sys:make-process #'run :name process-name)
                  frame)
          (run)))))
