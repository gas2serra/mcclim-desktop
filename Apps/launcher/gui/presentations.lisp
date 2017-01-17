(in-package :desktop-launcher)

(clim:define-presentation-method clim:present (app (type application)
						   stream
						   (view clim:textual-view)
						   &key)
  (with-accessors ((label application-pretty-name)) app
    (write-string label stream)))

(clim:define-presentation-method clim:accept ((type application) stream view &key)
  (values
   (clim:completing-from-suggestions (stream :partial-completers '(#\Space))
     (mapcar #'(lambda (o)
		 (clim:suggest (application-pretty-name o) o))
	     *applications*))))

