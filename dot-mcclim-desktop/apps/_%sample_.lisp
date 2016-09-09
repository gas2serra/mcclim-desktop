(in-package :mcclim-desktop-user)

;;;
;;; Examples
;;;
#|

(register-application "" 'standard-mcclim-application
		      :pretty-name ""
		      :icon nil
		      :home-page nil
		      :git-repo nil
		      :system-name "")

(register-application "" 'standard-shell-application
		      :pretty-name ""
		      :make-command-fn #'(lambda (&rest args)
					   ))

(register-application "" 'standard-alias-application
		      :pretty-name ""
		      :icon nil
		      :reference (find-application "")
		      )

|#


