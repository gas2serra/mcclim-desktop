(in-package :desktop-apropos)

;;;
;;; Constants
;;;

(defparameter *symbol-bounding-types* '(:variable :function :generic-function
				       :class :macro :setf :type))

;;;
;;; utility functions
;;;

(defun symbol-external-p (symbol)
  "Return t only if the symbol is external"
  (swank::symbol-external-p symbol))

(defun symbol-bound-to (symbol type)
  (ccase type
    (:variable
     (and symbol (boundp symbol)))
    (:function
     (fboundp symbol))
    (:macro
     (macro-function symbol))
    (:class
     (find-class symbol nil))
    (:generic-function
     (and (fboundp symbol)
	  (typep (symbol-function symbol) 'generic-function)))
    ((:setf :type)
     (not (eq (getf (swank/backend::describe-symbol-for-emacs symbol) type 'cl:t)
	      t)))))

(defun symbol-documentation (symbol type)
  (let ((doc (getf (swank/backend::describe-symbol-for-emacs symbol)
		   (case type
		     (:class
		      :type)
		     (:generic-function
		      #+sbcl :generic-function
		      #+ccl :function)
		     (:macro
		      #+sbcl :macro
		      #+ccl :function)
		     (otherwise
		      type))
		   'cl:t)))
    (if (member doc '(t nil :NOT-DOCUMENTED))
	nil
	doc)))

(defun list-symbol-bounding-types (symbol)
  (let ((types (remove-if #'(lambda (type)
			      (not (symbol-bound-to symbol type)))
			  *symbol-bounding-types*)))
    (cond
      ((member :generic-function types)
       (remove :function types))
      ((member :macro types)
       (remove :function types))
      (t
       types))))

(defun symbol-object (symbol type)
  (ccase type
    (:variable
     (symbol-value symbol))
    (:function
     (when (fboundp symbol)
       (symbol-function symbol)))
    (:macro
     (macro-function symbol))
    (:class
     (when (find-class symbol nil)
       (find-class symbol)))
    (:generic-function
     (when (fboundp symbol)
       (symbol-function symbol)))
    (:setf
     #+sbcl (swank/sbcl::setf-expander symbol)
     #+ccl nil)
    (:type
     nil)))

(defun symbol-location (symbol type)
  (let ((definitions (swank::find-definitions symbol)))
    (flet ((convert (ty)
	     (let ((loc 
		    (car (cdr (find-if #'(lambda (x) (eq ty (caar x))) definitions)))))
	       (when (eq (car loc) :location)
		 (cons (cadr (assoc :file (cdr loc)))
		       (cadr (assoc :position (cdr loc))))))))
      #+sbcl
      (ccase type
	(:variable
	 (convert 'defvar))
	(:function
	 (convert 'defun))
	(:generic-function
	 (convert 'defgeneric))
	(:macro
	 (convert 'defmacro))
	(:class
	 (convert 'defclass))
	(:setf
	 (convert 'define-setf-expander))
	(:type
	 (convert 'defclass)))
      #+ccl
      (ccase type
	(:variable
	 (convert 'variable))
	(:function
	 (convert 'defun))
	(:generic-function
	 (convert 'defgeneric))
	(:macro
	 (convert 'defmacro))
	(:class
	 (convert 'defclass))
	(:setf
	 (convert 'define-setf-expander))
	(:type
	 (convert 'defclass))))))
	
(defun symbol-description (symbol type)
  (with-output-to-string (*standard-output*)
    (case type
      ((:variable nil)
       (describe symbol))
      (:function
       (when (fboundp symbol)
	 (describe (symbol-function symbol))))
      (:macro
       (when (macro-function symbol)
	 (describe (macro-function symbol))))
      (:class
       (when (find-class symbol nil)
	 (describe (find-class symbol))))
      (:generic-function
       (when (fboundp symbol)
	 (describe (symbol-function symbol))))
      (:setf
       #+sbcl (when (sb-int:info :setf :expander symbol)
		(describe (sb-int:info :setf :expander symbol)))
       #+ccl (describe (ccl:setf-function-spec-name `(setf ,symbol))))
      (:type
       #+sbcl (describe (sb-kernel:values-specifier-type symbol))
       #+ccl (describe (or (find-class symbol nil) symbol))))))
