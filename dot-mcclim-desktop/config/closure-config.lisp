(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app &rest args)
	  (declare (ignore app args))
	  (closure:start)
	  (bt:join-thread clim-gui::*closure-process*)))

(setf gui:*home-page* "http://www.cliki.net/")



