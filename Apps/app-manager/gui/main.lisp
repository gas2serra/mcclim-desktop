(in-package :desktop-app-manager)

(defun run-app-manager (&key (new-process nil)
		       (width nil)
		       (height nil)
		       port
		       frame-manager
		       (pretty-name "Desktop App-Manager")
		       (process-name "desktop-app-manager"))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
         (frame (clim:make-application-frame 'desktop-app-manager
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
