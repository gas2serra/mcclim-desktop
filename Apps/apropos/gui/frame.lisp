(in-package :desktop-apropos)


;;;
;;; utility
;;;

(defun string-to-keyword (str)
  (if (string= str "nil")
      nil
      (intern (string-upcase str) :keyword)))

;;;
;;; application frame
;;;

(clim:define-application-frame apropos-navigator ()
  ((selected-values :initform nil)
   (selected-result-options :initform '(:fully-qualified))
   (selected-output-option :initform ':selection)
   (selected-action-option :initform ':single)
   (symbol-view :initform +fully-qualified-symbol-view+)
   (iapropos :initform (make-instance 'iapropos)))
  (:command-table (apropos-navigator
		   :inherit-from (edit-menu)
		   :menu (("Edit" :menu edit-menu))))
  (:menu-bar t)
  (:panes
   (symbol-regex-text-field :text-field
			    :value-changed-callback '%update-matching-symbols)
   (package-regex-text-field :text-field
			     :value-changed-callback '%update-matching-packages)
   (symbol-result-display :application
			  :incremental-redisplay t
			  :display-function '%render-symbol-result
			  :display-time nil
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 250)
   (package-result-display :application
			  :incremental-redisplay t
			  :display-function '%render-package-result
			  :display-time nil
			  :scroll-bars :vertical
			  :end-of-line-action :allow
			  :end-of-page-action :allow
			  :min-width 190
			  :max-width 400)
   (output-display :application
		   :incremental-redisplay t
		   :display-function '%render-output
		   :scroll-bars :vertical
		   :end-of-page-action :allow)
   (result-options
    (clim:with-radio-box (:type :some-of
			  :orientation :horizontal
			  :value-changed-callback '%update-result-options)
      (clim:radio-box-current-selection "fully-qualified")))
   (output-option
    (clim:with-radio-box (:orientation :horizontal
			  :value-changed-callback '%update-output-option)
      (clim:radio-box-current-selection "selection")
      "documentation" "location" "description" "object"))
   (external-option
    (clim:with-radio-box (:orientation :horizontal
			  :value-changed-callback '%update-external-option)
      "yes"
      "no"
      (clim:radio-box-current-selection "nil")))
   (documentation-option
    (clim:with-radio-box (:orientation :horizontal
			  :value-changed-callback '%update-documentation-option)
      "yes"
      "no"
      (clim:radio-box-current-selection "nil")))
   (bound-to-option
    (clim:with-radio-box (:orientation :vertical
			  :value-changed-callback '%update-bound-to-option)
      (clim:radio-box-current-selection "nil") "variable"
      "function" "class" "generic-function" "macro"
      "setf" "type"))
   (subclass-option :option-pane
		    :value nil
		    :active nil
		    :items *apropos-navigator-subclas-of-options*
		    :name-key #'car
		    :value-key #'cdr	    
		    :value-changed-callback #'%update-subclass-option)
   (metaclass-option :option-pane
		     :value nil
		     :active nil
		     :items *apropos-navigator-metaclas-of-options*
		     :name-key #'car
		     :value-key #'cdr	
		     :value-changed-callback #'%update-metaclass-option)
   (preselect-option :option-pane
		     :value nil
		     :items *apropos-navigator-preselect-options*
		     :name-key #'car
		     :value-key #'cdr
		     :value-changed-callback #'%update-preselect-option)
   (action-option
    (clim:with-radio-box (:orientation :horizontal
			  :value-changed-callback '%update-action-option)
      (clim:radio-box-current-selection "single") "multiple"))
   (return-action :push-button
		  :activate-callback #'(lambda (gadget)
					 (declare (ignore gadget))
					 (com-quit))
		  :label "return")
   (copy-action :push-button
		:activate-callback #'(lambda (gadget)
				       (declare (ignore gadget))
				       (com-edit-copy-to-clipboard))
		:label "clipboard")
   (kill-ring-action :push-button 
		     :activate-callback #'(lambda (gadget)
					    (declare (ignore gadget))
					    (com-edit-copy-to-kill-ring))
		     :label "kill-ring"))
  (:layouts
   (:default
       (clim:horizontally nil
	 (clim:vertically nil
	   (clim:labelling (:label "Symbol")
	     (clim:vertically nil
	       (clim:labelling (:label "bound to")
		 bound-to-option)
	       (clim:labelling (:label "external")
		 external-option)
	       (clim:labelling (:label "documentation")
		 documentation-option)))
	   (clim:labelling (:label "Class")
	     (clim:vertically nil
	       (clim:labelling (:label "subclass of")
		 subclass-option)
	       (clim:labelling (:label "metaclass of")
		 metaclass-option)))
	   (clim:labelling (:label "Preselect")
	     preselect-option)
	   (clim:labelling (:label "Selection")
	     action-option)
	   (clim:+fill+))
	 (clim:+fill+
	  (clim:vertically nil
	    (2/3 (clim:labelling (:label "Results")
		   (clim:vertically nil
		     result-options
		     (clim:horizontally nil
		       (2/5 package-result-display)
		       (3/5 symbol-result-display)))))
	    (1/3 (clim:labelling (:label "Output")
		   (clim:vertically nil
		     output-option
		     output-display)))
	    (clim:horizontally nil
	      (clim:labelling (:label "Copy to")
		(clim:vertically nil
		  copy-action
		  kill-ring-action))
	      (clim:+fill+
	       (clim:vertically nil
		 (clim:labelling (:label "symbol apropos" :align-x :center)
		   symbol-regex-text-field)
		 (clim:labelling (:label "package apropos" :align-x :center)
		   package-regex-text-field))))))))))

;;;
;;; frame initialization
;;;

(defmethod clim::note-frame-enabled ((fm clim:frame-manager) (frame apropos-navigator))
  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) nil)
  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) nil))

;;;
;;; input/output
;;;

(defmethod clim:frame-standard-input ((frame apropos-navigator))
  (car (clim:sheet-children
	(clim:find-pane-named clim:*application-frame* 'symbol-regex-text-field))))

;;;
;;; callbacks
;;;

(defun %update-matching-packages (this-gadget value)
  (declare (ignore this-gadget))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
    (clim:window-clear anaphora:it))
  (with-slots (iapropos) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(%print-error
			 condition
			 (clim:find-pane-named clim:*application-frame* 'output-display))
			(%maybe-update-package-result-display)
			(%maybe-update-symbol-result-display)
			(return-from %update-matching-packages))))
      (setf (iapropos-package-text iapropos) value)))
  (%maybe-update-package-result-display)
  (%maybe-update-symbol-result-display))
  
(defun %update-matching-symbols (this-gadget value)
  (declare (ignore this-gadget))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
    (clim:window-clear anaphora:it))
  (with-slots (iapropos) clim:*application-frame*
    (handler-bind ((cl-ppcre:ppcre-syntax-error
		    #'(lambda (condition)
			(%print-error
			 condition
			 (clim:find-pane-named clim:*application-frame* 'output-display))
			(%maybe-update-package-result-display)
			(%maybe-update-symbol-result-display)
			(return-from %update-matching-symbols))))
      (setf (iapropos-symbol-text iapropos) value)))
  (%maybe-update-symbol-result-display))

(defun %update-bound-to-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-bound-to iapropos)
	  (string-to-keyword (clim:gadget-label selected-gadget))))
  (if (string= (clim:gadget-label selected-gadget) "class")
      (progn
	(clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'subclass-option))
	(clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'metaclass-option)))
      (progn
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'subclass-option))
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'metaclass-option))))
  (if (string/= (clim:gadget-label selected-gadget) "nil")
      (clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-option))
      (clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'documentation-option)))
  (%maybe-update-symbol-result-display))

(defun %update-external-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-external-yes/no iapropos)
	  (string-to-keyword (clim:gadget-label selected-gadget))))
  (%maybe-update-symbol-result-display))

(defun %update-documentation-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-documentation-yes/no iapropos)
	  (string-to-keyword (clim:gadget-label selected-gadget))))
  (%maybe-update-symbol-result-display))

(defun %update-subclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-subclass-of iapropos) selected-value))
  (%maybe-update-symbol-result-display))

(defun %update-metaclass-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (with-slots (iapropos) clim:*application-frame*
    (setf (iapropos-metaclass-of iapropos) selected-value))
  (%maybe-update-symbol-result-display))

(defun %update-preselect-option (this-gadget selected-value)
  (declare (ignore this-gadget))
  (if selected-value
      (progn
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'bound-to-option))
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'subclass-option))
	(clim:deactivate-gadget (clim:find-pane-named clim:*application-frame* 'metaclass-option))
	(with-slots (iapropos) clim:*application-frame*
	  (funcall selected-value iapropos)))
      (progn
	(setf (clim:gadget-value
	       (clim:find-pane-named clim:*application-frame* 'bound-to-option)) "nil")
	(setf (clim:gadget-value
	       (clim:find-pane-named clim:*application-frame* 'subclass-option)) nil)
	(setf (clim:gadget-value
	       (clim:find-pane-named clim:*application-frame* 'metaclass-option)) nil)
	(clim:activate-gadget (clim:find-pane-named clim:*application-frame* 'bound-to-option))
	(with-slots (iapropos) clim:*application-frame*
	  (setf (iapropos-filter-fn iapropos) nil)
	  (setf (iapropos-subclass-of iapropos) nil)
	  (setf (iapropos-metaclass-of iapropos) nil)
	  (setf (iapropos-bound-to iapropos) nil))))
  (%maybe-update-symbol-result-display))

(defun %update-output-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-output-option) clim:*application-frame*
    (setf selected-output-option 
	  (intern (string-upcase (clim:gadget-label selected-gadget)) :keyword)))
  (%maybe-update-output-display))

(defun %update-result-options (this-gadget selected-gadgets)
  (declare (ignore this-gadget))
  (with-slots (selected-result-options symbol-view) clim:*application-frame*
    (setf selected-result-options nil)
    (dolist (sg selected-gadgets)
      (push 
       (string-to-keyword (clim:gadget-label sg))
       selected-result-options))
    (if (member :fully-qualified selected-result-options)
	(setf symbol-view +fully-qualified-symbol-view+)
	(setf symbol-view clim:+textual-view+)))
  (%maybe-update-symbol-result-display))

(defun %update-action-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (selected-action-option selected-values) clim:*application-frame*
    (setf selected-action-option 
	  (string-to-keyword (clim:gadget-label selected-gadget)))
    (if (eq selected-action-option :single)
	(progn
	  (setf selected-values (when selected-values (list (car selected-values))))
	  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) nil)
	  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) nil)
	  (%maybe-update-symbol-result-display)
	  (%maybe-update-output-display))
	(progn
	  (setf (clim:command-enabled 'com-edit-select-all clim:*application-frame*) t)
	  (setf (clim:command-enabled 'com-edit-select-none clim:*application-frame*) t)))))

(defun %maybe-update-symbol-result-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'symbol-result-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))

(defun %maybe-update-package-result-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'package-result-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))

(defun %maybe-update-output-display (&rest _)
  (declare (ignore _))
  (anaphora:awhen (clim:find-pane-named clim:*application-frame* 'output-display)
    (clim:redisplay-frame-pane clim:*application-frame* anaphora:it :force-p t)))

;;;
;;; render functions
;;;

(defun %print-heading-text (text pane)
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane 10 5)
  (clim:surrounding-output-with-border
   (pane :shape :underline :ink clim:+black+)
   (clim:with-text-style (pane *apropos-navigator-heading-text-style*)
     (princ text pane))))

(defun %print-sub-heading-text (text pane)
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane 10 5)
  (clim:surrounding-output-with-border
   (pane :shape :underline :ink clim:+black+)
   (clim:with-text-style (pane *apropos-navigator-sub-heading-text-style*)
     (princ text pane))))

(defun %print-error (condition pane)
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane 10 5)
  (clim:with-drawing-options (pane :ink clim:+red+)
    (%print-heading-text "Syntax Error" pane)
    (fresh-line pane)
    (clim:stream-increment-cursor-position pane 10 0)
    (princ condition pane)))

(defun %print-text (pane text &optional (x-offset 0))
  (fresh-line pane)
  (clim:stream-increment-cursor-position pane (+ x-offset 10) 0)
  (princ text pane))

(defun take (n l)
  (subseq l 0 (if (< (length l) n) (if (> n (1- (length l))) (length l) (1- (length l))) n)))

(defun %render-output (frame pane)
  (declare (ignore frame))
  (declare (ignore frame))
  (with-slots (selected-values selected-output-option iapropos) clim:*application-frame*
    (flet ((print-symbol (sym type opt)
	     (ccase opt
	       (:selection
		(%print-sub-heading-text (format nil "Selected symbols") pane)
		(let ((*print-escape* t))
		  (dolist (v selected-values)
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present v 'symbol :stream pane :view +fully-qualified-symbol-view+)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (anaphora:awhen (list-symbol-bounding-types v)
		      (princ anaphora:it pane)))))
	       (:object
		(%print-sub-heading-text (format nil "Object (~A)" type) pane)
		(let ((obj (symbol-object sym type)))
		  (when obj
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present obj 'object
				  :stream pane :view clim:+textual-view+))))
	       (:location
		(%print-sub-heading-text (format nil "Location (~A)" type) pane)
		(let ((loc (symbol-location sym type)))
		  (when loc
		    (fresh-line pane)
		    (clim:stream-increment-cursor-position pane 10 0)
		    (clim:present loc 'source-location
				  :stream pane :view clim:+textual-view+))))
	       (:documentation
		(%print-sub-heading-text (format nil "Documentation (~A)" type) pane)
		(let ((doc (symbol-documentation (car selected-values) type)))
		  (when doc
		    (%print-text pane doc))))
	       (:description
		(%print-sub-heading-text (format nil "Description (~A)" type) pane)
		(let ((des (symbol-description (car selected-values) type)))
		  (when des
		    (%print-text pane des)))))
	     (clim:stream-increment-cursor-position pane 0 5)))
      (if (null selected-values)
	  (%print-heading-text (format nil "Empty selection") pane)
	  (if (eq selected-output-option :selection)
	      (print-symbol selected-values nil selected-output-option)
	      (dolist (sym selected-values)
		(let ((*print-escape* t))
		  (%print-heading-text (format nil "~S" sym) pane))
		(if (iapropos-bound-to iapropos) 
		    (print-symbol sym
				  (iapropos-bound-to iapropos) selected-output-option)
		    (dolist (type (list-symbol-bounding-types sym))
		      (print-symbol sym type selected-output-option)))))))))

(defun %render-symbol-result (frame pane)
  (declare (ignore frame))
  (with-slots (iapropos selected-values symbol-view)
      clim:*application-frame*
    (let* ((matching-symbols (iapropos-matching-symbols iapropos))
	   (symbols-to-print (take 400 matching-symbols)))
      (%print-heading-text (format nil "Symbols (~A/~A~A)"
				   (length symbols-to-print)
				   (length matching-symbols)
				   (if (iapropos-result-overflow-p iapropos) "*" "")) pane)
      (if (null matching-symbols)
	  (progn
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (princ "; no results" pane))
	  (dolist (sym symbols-to-print)
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 10 0)
	    (clim:with-drawing-options (pane :ink
					     (if (member sym selected-values)
						 clim:+blue+
						 clim:+black+))
	      (clim:present sym 'symbol :stream pane :view symbol-view)))))))

(defun %render-package-result (frame pane)
  (declare (ignore frame))
  (with-slots (iapropos)
      clim:*application-frame*
    (let* ((matching-packages (iapropos-matching-packages iapropos)))
      (%print-heading-text (format nil "Packages (~A)" (length matching-packages)) pane)
      (if (null matching-packages)
	  (progn
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (princ "; no results" pane))
	  (dolist (package matching-packages)
	    (fresh-line pane)
	    (clim:stream-increment-cursor-position pane 5 0)
	    (clim:present package 'package :stream pane :view clim:+textual-view+))))))

;;;
;;; return values
;;;

(defvar *return-values* nil)

(defun %update-return-values ()
  (with-slots (selected-values selected-action-option iapropos) clim:*application-frame*
    (setf *return-values*
	  (ccase selected-action-option
	    (:single
	     (car selected-values))
	    (:multiple
	     (remove-duplicates selected-values))))))

;;;
;;;
;;;

(defmacro with-fixed-vertical-scroll-bar (pane &body body)
  (let ((vscrollbar (gensym "VSCROLLBAR"))
	(vsb-value (gensym "VSB-VALUE")))
    `(let* ((,vscrollbar (slot-value (clim:sheet-parent
				     (clim:sheet-parent
				     ,pane))
				    'climi::vscrollbar))
	    (,vsb-value (slot-value ,vscrollbar 'climi::value)))
       ,@body
       (progn
	 (climi::drag-callback ,vscrollbar nil nil ,vsb-value)
	 (setf (clim:gadget-value ,vscrollbar) ,vsb-value)))))
