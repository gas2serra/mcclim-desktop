(in-package :desktop-sys)

(defun copy-to-x11-clipboard (string)
  (with-input-from-string (input-stream
			   (coerce string 'string))
    (uiop:run-program "xclip -selection clipboard -i "
		      :output nil :input input-stream)))

(defun paste-from-x11-clipboard ()
  (uiop:run-program "xclip -selection clipboard -o" :output :string))
