(in-package :desktop-internals)

(defparameter *show-resource-padding* 10)

(defmacro with-resource-show-output (stream &body body)
`(progn
   (fresh-line ,stream)
   (clim:stream-increment-cursor-position ,stream 0 20)
   (clim:surrounding-output-with-border
       (,stream
	:shape :rounded
	:padding 10
	:ink clim:+green+
	:background clim:+grey80+)
     ,@body)))

(defun show-fresh-line (stream)
  (fresh-line stream)
  (clim:stream-increment-cursor-position stream (* 2 *show-resource-padding*) 0))

(defun show-resource (thing &optional stream)
  (if (null stream)
      (setq stream *standard-output*)
      (if  (eq stream t)
           (setq stream *terminal-io*)))
  (with-resource-show-output stream
    (show-object thing stream))
  (values))

(defun list-resources (things &optional stream)
  (if (null stream)
      (setq stream *standard-output*)
      (if  (eq stream t)
           (setq stream *terminal-io*)))
  (with-resource-show-output stream
    (list-objects things stream))
  (values))



(defgeneric show-object (thing stream))

(defmethod show-object (thing stream)
  (show-fresh-line stream)
  (clim:present thing (desktop-presentation-type-of thing) :stream stream))

(defgeneric list-objects (things stream))

(defmethod list-objects (things stream)
  (dolist (object things)
    (show-object object stream)))
