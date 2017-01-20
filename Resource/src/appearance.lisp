(in-package :desktop-internals)


(defmacro with-resource-list-output ((stream) &body body)
  `(progn
     (clim:stream-increment-cursor-position ,stream 0 20)
     (clim:surrounding-output-with-border
	 (,stream
	  :shape :rounded
	  :padding 10
	  :ink clim:+orange+
	  :background clim:+grey80+)
       ,@body)))

(defmacro with-resource-list-item-output ((stream) &body body)
  `(progn
     (fresh-line ,stream)
     (clim:stream-increment-cursor-position ,stream 20 0)
     ,@body))

(defmacro with-resource-show-output ((stream) &body body)
  `(progn
     (fresh-line ,stream)
     (clim:stream-increment-cursor-position ,stream 0 20)
     (clim:surrounding-output-with-border
	 (,stream
	  :shape :rounded
	  :padding 10
	  :ink clim:+green+
	  :background clim:+grey100+)
       (fresh-line ,stream)
       (clim:stream-increment-cursor-position ,stream 20 0)
       ,@body)))
