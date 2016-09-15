(in-package :desktop-user)

(when *application*
  (setf (application-entry-fn *application*)
        #'(lambda (app &rest args)
            (declare (ignore app args))
            (beirc:beirc :new-process nil))))

(defmethod irc::nickname ((u (eql nil)))
  nil)
