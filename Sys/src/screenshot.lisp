(in-package :desktop-sys)

(defun x11-screenshot (pathname)
  (let ((id (string-trim '(#\Space #\Tab #\Newline) 
			 (uiop:run-program "xprop -root | grep '_NET_ACTIVE_WINDOW(WINDOW)' | awk -F ' ' '{print $5}' | tr ',' ' '"
					   :output :string))))
    (uiop:run-program (format nil "import -window ~A ~A" id
			      (namestring pathname)))))

