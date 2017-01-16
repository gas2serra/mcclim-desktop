(in-package :desktop-task-manager)

(defun run-task-manager (&key (new-process nil)
			   (width 790)
			   (height 550)
			   port
			   frame-manager
			   (pretty-name "Task Manager")
			   (process-name "task-manager"))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
         (frame (clim:make-application-frame 'task-manager
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
