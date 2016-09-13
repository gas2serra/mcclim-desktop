(in-package :desktop-user)

(load-application (find-application :clouseau))
(load (merge-pathnames "Apps/Debugger/clim-debugger.lisp"
		       (asdf:component-pathname (asdf:find-system "mcclim"))))

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'clim-debugger:debugger
		   condition me-or-my-encapsulation)))
      
