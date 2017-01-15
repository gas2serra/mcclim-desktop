(in-package :desktop-sys)

(defun copy-to-kill-ring (string)
  (drei-kill-ring:kill-ring-standard-push
   drei-kill-ring:*kill-ring*
   string))

(defun paste-from-kill-ring ()
  (coerce (drei-kill-ring:kill-ring-yank
	   drei-kill-ring:*kill-ring*)
	  'string))
