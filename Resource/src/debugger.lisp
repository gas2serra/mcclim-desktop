(in-package :desktop-internals)

(defclass minimized-stack-frame-view (clim:textual-view)())
(defclass maximized-stack-frame-view (clim:textual-view)())

(defparameter +minimized-stack-frame-view+ 
  (make-instance 'minimized-stack-frame-view))
(defparameter +maximized-stack-frame-view+ 
  (make-instance 'maximized-stack-frame-view))


;;;
;;; Clim resource for debugger
;;;

(defclass debugger-info ()
  ((the-condition :accessor the-condition
		  :initarg :the-condition)
   (condition-message :accessor condition-message
		      :initarg  :condition-message)
   (type-of-condition :accessor type-of-condition
		      :initarg  :type-of-condition)
   (condition-extra :accessor condition-extra
		    :initarg  :condition-extra)
   (restarts :accessor restarts
	     :initarg :restarts)
   (backtrace :accessor backtrace
	      :initarg :backtrace)))


(defclass stack-frame ()
  ((clim-view       :accessor view :initform +minimized-stack-frame-view+)
   (frame-string    :accessor frame-string
		    :initarg  :frame-string)
   (frame-no        :accessor frame-no
		    :initarg :frame-no)
   (frame-variables :accessor frame-variables
		    :initarg :frame-variables)))

(defun compute-backtrace (start end)
  (loop for frame    in   (swank-backend::compute-backtrace start end)
     for frame-no from 0
     collect (make-instance
	      'stack-frame
	      :frame-string    (let ((*print-pretty* nil))
				 (with-output-to-string (stream) 
				   (swank-backend::print-frame frame stream)))
	      :frame-no        frame-no
	      :frame-variables (swank-backend::frame-locals frame-no))))

(defmethod expand-backtrace ((info debugger-info) (value integer))
  (with-slots (backtrace) info
    (setf backtrace (compute-backtrace 0 (+ (length backtrace) 10)))))


(defun make-debugger-info (condition)
  (make-instance 
   'debugger-info
   :the-condition        condition
   :type-of-condition    (type-of condition)
   :condition-message    (swank::safe-condition-message condition)
   :condition-extra      (swank::condition-extras       condition)
   :restarts             (compute-restarts)
   :backtrace            (compute-backtrace 0 20)))


;;;
;;; presentations
;;; 

(clim:define-presentation-type restart ())


;; Used to provide the clim frame with the condition info that
;; triggered the debugger.

(defparameter *condition* nil)

(defmacro bold ((stream) &body body)
  `(clim:with-text-face (,stream :bold)
     ,@body))


(clim:define-presentation-method clim:present (object (type restart) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (bold (stream) (format t "~A" (restart-name object))))

(clim:define-presentation-method clim:accept ((type restart) stream view &key)
  (declare (ignore view))
  (let ((condition-info *condition*))
    (values
     (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
       (loop
	 for r in (restarts condition-info)
	 for i from 0 
	  do 
	    (clim:suggest (format nil "~A: ~A" i (restart-name r)) r))))))
