(in-package :desktop-user)

(setf (application-entry-fn *application*)
      #'(lambda (app condition me-or-my-encapsulation)
	  (declare (ignore app))
	  (funcall #'panter-apps:debugger
		   condition
		   me-or-my-encapsulation)))

(setf (application-debugger-fn *application*)
      #'panter-apps:debugger)

(defmethod desktop:use-debugger ((debugger function))
  (setf desktop-extensions:*desktop-debugger-hook* panter::*debugger-hook*)
  (setf *debugger-hook* panter::*debugger-hook*)
  (setf panter:*debugger* debugger))

