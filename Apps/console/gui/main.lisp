(in-package :desktop-console)

(defun run-console (&key (new-process nil)
		       (width nil)
		       (height nil)
		       port
		       frame-manager
		       (pretty-name "Desktop Console")
		       (process-name "desktop-console"))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
         (frame (clim:make-application-frame 'desktop-console
					     :pretty-name pretty-name
					     :frame-manager fm
					     :width width
					     :height height)))
    (flet ((run () 
	     (unwind-protect (clim:run-frame-top-level frame)
	       (clim:disown-frame fm frame))))
      (if new-process
          (values (clim-sys:make-process #'run :name process-name)
                  frame)
          (run)))))
