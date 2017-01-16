(in-package :desktop-apropos)


;;;
;;; symbol
;;;

(defclass fully-qualified-symbol-view (clim:textual-view)
  ())

(defparameter +fully-qualified-symbol-view+
  (make-instance 'fully-qualified-symbol-view))

(clim:define-presentation-method clim:present (object (type symbol) stream
						      (view fully-qualified-symbol-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((*package* (find-package :common-lisp-user)))
    (prin1 object stream)))

;;;
;;; package
;;;


;;(clim:define-presentation-type package ())

(clim:define-presentation-method clim:present (object (type package) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (package-name object) stream))

;;;
;;; object
;;;

(clim:define-presentation-type object ())

(clim:define-presentation-method clim:present (object (type object) stream
						      (view clim:textual-view)
						      &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ object stream))

;;;
;;; location
;;;

(clim:define-presentation-type source-location ())

(clim:define-presentation-method clim:present (loc (type source-location) stream
						   (view clim:textual-view)
						   &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (princ (format nil "~A:~A" (car loc) (cdr loc)) stream))
