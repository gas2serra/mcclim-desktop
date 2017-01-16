(in-package :desktop-apropos)

;;;
;;; parameters
;;;

(defparameter *default-iapropos-max-result-length* 1000
  "The max length of the result")

;;;
;;; interactive apropos
;;;

(defclass iapropos ()
  ((symbol-text :initform nil
		:accessor iapropos-symbol-text)
   (cached-symbol-scanner :initform nil)
   (package-text :initform ""
		 :accessor iapropos-package-text)
   (cached-package-scanner :initform nil)
   (external-yes/no :type '(member nil :yes :no)
		    :initform nil
		    :accessor iapropos-external-yes/no)
   (documentation-yes/no :type '(member nil :yes :no)
			 :initform nil
			 :accessor iapropos-documentation-yes/no)
   (bound-to :type '(member nil
		       :variable :function :generic-function
		       :class :macro
		       :setf :type)
	     :initform nil
	     :accessor iapropos-bound-to)
   (subclass-of :initform nil
		:accessor iapropos-subclass-of)
   (metaclass-of :initform nil
		 :accessor iapropos-metaclass-of)
   (filter-fn :initform nil
	      :accessor iapropos-filter-fn)
   (max-result-length :initform *default-iapropos-max-result-length*
		      :accessor iapropos-max-result-length)
   (result-overflow-p :initform nil
		      :reader iapropos-result-overflow-p)
   (cached-matching-packages :initform (list-all-packages))
   (cached-matching-symbols :initform nil))
  (:documentation "Interactive apropos class based on cl-ppcre and swank"))

;;; generic funtions
(defgeneric iapropos-matching-symbols (iapropos)
  (:documentation "Return the list of symbols that match the specified criteria"))
(defgeneric iapropos-matching-packages (iapropos)
  (:documentation "Return the list of packages that match the specified criteria"))
(defgeneric iapropos-matching-symbol-p (iapropos symbol))

;;; methods
(defmethod (setf iapropos-symbol-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (symbol-text cached-symbol-scanner 
			   cached-matching-symbols) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf cached-symbol-scanner nil)
			(setf cached-matching-symbols nil))))
      (setf cached-symbol-scanner (when (and symbol-text (string/= symbol-text ""))
				    (cl-ppcre:create-scanner
				     symbol-text :case-insensitive-mode t)))
      (%iapropos-update-matching-symbols iapropos))))

(defmethod (setf iapropos-package-text) :after (text (iapropos iapropos))
  (declare (ignore text))
  (with-slots (package-text cached-package-scanner
			    cached-matching-symbols
			    cached-matching-packages) iapropos
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(setf cached-package-scanner nil)
			(setf cached-matching-packages nil)
			(setf cached-matching-symbols nil))))
      (setf cached-package-scanner (when (and package-text (string/= package-text ""))
				     (cl-ppcre:create-scanner
				      package-text :case-insensitive-mode t)))
      (%iapropos-update-matching-packages iapropos))))

(defmethod (setf iapropos-external-yes/no) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-bound-to) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-documentation-yes/no) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-subclass-of) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-metaclass-of) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod (setf iapropos-filter-fn) :after (val (iapropos iapropos))
  (declare (ignore val))
  (%iapropos-update-matching-symbols iapropos))

(defmethod iapropos-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages) iapropos
    cached-matching-packages))

(defmethod iapropos-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-symbols) iapropos
    cached-matching-symbols))

(defmethod iapropos-matching-symbol-p ((iapropos iapropos) symbol)
  (%iapropos-matching-symbol-p iapropos symbol))

;;;
;;; private generic functions
;;;

(defgeneric %iapropos-update-matching-symbols (iapropos))
(defgeneric %iapropos-update-matching-packages (iapropos))

;;; methods
(defmethod %iapropos-update-matching-packages ((iapropos iapropos))
  (with-slots (cached-matching-packages cached-package-scanner) iapropos
    (setf cached-matching-packages
	  (sort
	   (if (null cached-package-scanner)
	       (list-all-packages)
	       (let ((out))
		 (dolist (p (list-all-packages))
		   (when (cl-ppcre:scan cached-package-scanner (package-name p))
		     (push p out)))
		 out))
	   #'string< :key #'package-name)))
  (%iapropos-update-matching-symbols iapropos))

(defmethod %iapropos-update-matching-symbols ((iapropos iapropos))
  (with-slots (cached-matching-packages
	       cached-matching-symbols
	       max-result-length
	       result-overflow-p) iapropos
    (setf cached-matching-symbols
	  (let ((swank::*buffer-package* (find-package :common-lisp-user))
		(swank::*buffer-readtable* *readtable*)
		(out)
		(i 0))
	    (setf result-overflow-p nil)
	    (block iter
	      (with-package-iterator (next cached-matching-packages :external :internal)
		(loop (multiple-value-bind (morep symbol) (next)
			(when (not morep)
			  (return-from iter))
			(when (= i max-result-length)
			  (setf out (remove-duplicates out))
			  (setf i (length out))
			  (when (= i max-result-length)
			    (setf result-overflow-p t)
			    (return-from iter)))
			(when (%iapropos-matching-symbol-p iapropos symbol)			      
			  (push symbol out)
			  (incf i))))))
	    (sort
	     (remove-duplicates out)
	     #'swank::present-symbol-before-p)))))

;;;
;;; private functions
;;;

(defun %iapropos-matching-symbol-p (iapropos symbol)
  (with-slots (cached-symbol-scanner external-yes/no documentation-yes/no bound-to
	       subclass-of metaclass-of filter-fn) iapropos
    (and
     (if external-yes/no
	 (eq (not (symbol-external-p symbol))
	     (eq external-yes/no :no))
	 t)
     (if bound-to
	 (symbol-bound-to symbol bound-to)
	 t)
     (if (eq bound-to :class)
	 (and
	  (if subclass-of
	      (subtypep symbol subclass-of)
	      t)
	  (if metaclass-of
	      (subtypep (type-of (find-class symbol)) metaclass-of)
	      t))
	 t)
     (if filter-fn
	 (funcall filter-fn symbol)
	 t)
     (if (and bound-to documentation-yes/no)
	 (eq (not (symbol-documentation symbol bound-to))
	     (eq documentation-yes/no :no))
	 t)
     (if cached-symbol-scanner
	 (cl-ppcre:scan cached-symbol-scanner (symbol-name symbol))
	 t))))
