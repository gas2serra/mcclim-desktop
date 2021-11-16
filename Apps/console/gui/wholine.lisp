(in-package :desktop-console)

;;; parameters

(defparameter *wholine-text-style* (clim:make-text-style :sans-serif :roman :normal))
(defparameter *wholine-background* clim:+gray90+)
(defparameter *wholine-padding* 5)

;;; Presentation types
(clim:define-presentation-type bytes () :inherit-from 'integer)

(clim:define-presentation-method clim:present (object (type bytes)
						      stream (view clim:textual-view)
						      &key &allow-other-keys)
  (if (zerop object)
      (princ "0" stream)
      (let* ((suffixes '(" bytes" " KB" " MB" " GB" " TB" " PB"))
	     (x (floor (realpart (log object 1000))))
	     (idx (min x (1- (length suffixes)))))
	(if (zerop idx)
	    (format stream "~A bytes" object)
	    (format stream "~,1F~A" (/ object (expt 1000 idx)) (nth idx suffixes))))))


;; Wholine Pane

(defclass wholine-pane (clim:application-pane)
  ()
  (:default-initargs :background *wholine-background*
    :display-function #'display-wholine
    :scroll-bars nil
    :display-time :command-loop
    :end-of-line-action :allow))

(defmethod clim:compose-space ((pane wholine-pane) &key width height)
  (declare (ignore width height))  
  (let ((h (+ (* 2 *wholine-padding*) (clim:text-style-height *wholine-text-style* pane))))
    (clim:make-space-requirement :height h
				 :min-height h
				 :max-height h)))

(defun display-wholine (frame stream-pane)
  (let ((record (clim:with-output-to-output-record (stream-pane)
		  (let* ((*standard-output* stream-pane))
		    (generate-wholine-contents frame)))))
    (clim:with-bounding-rectangle* (rx0 ry0 rx1 ry1) (clim:bounding-rectangle record)
      (declare (ignore rx1 ry1))
      (setf (clim:output-record-position record)
	    (values (+ rx0 *wholine-padding*) (+ ry0 *wholine-padding*))))
    (clim:add-output-record record (clim:stream-output-history stream-pane))
    (clim:replay-output-record record stream-pane)))

;;;
;;; 
;;;

(defun print-package-name (stream)
  (let ((foo (package-name *package*)))
    (clim:with-drawing-options (stream :ink clim:+royalblue+)
      (format stream "~A" (reduce (lambda (&optional (a foo) (b foo))
                                    (if (< (length a) (length b)) a b))
                                  (package-nicknames *package*))))))

(defun frob-pathname (pathname)
  (namestring (truename pathname)))

(defun generate-wholine-contents (frame)
  (declare (ignore frame))
  (let* ((username (or (osicat:environment-variable "USER")
                       "luser"))
         (sitename (machine-instance))
	 (memusage #+(or cmu scl) (lisp::dynamic-usage)
                   #+sbcl  (sb-kernel:dynamic-usage)
                   #+lispworks (getf (system:room-values) :total-allocated)
		   #+openmcl (+ (ccl::%usedbytes) (ccl::%freebytes))
                   #+clisp (values (sys::%room))
                   #-(or cmu scl sbcl lispworks openmcl clisp) 0))
    (clim:with-text-style (t *wholine-text-style*)
      (clim:formatting-table (t :x-spacing '(2 :character))
        (clim:formatting-row (t)
          (macrolet ((cell ((align-x) &body body)                         
                       `(clim:formatting-cell (t :align-x ,align-x) ,@body)))
            (cell (:left)   (format t "~A@~A" username sitename))
	    (cell (:center)
		  (when (numberp memusage)
		    (clim:present memusage 'bytes)))
	    (cell (:center)
		  (format t "~A threads" (length (bt:all-threads))))
            (cell (:center)
                  ;; CLISP gives us an error when calling
                  ;; `cl:probe-file' with a directory argument.
		  (when #+clisp (or (ignore-errors (ext:probe-directory *default-pathname-defaults*))
				    (ignore-errors (probe-file *default-pathname-defaults*)))
			#-clisp (probe-file *default-pathname-defaults*)
			(clim:with-output-as-presentation (t (truename *default-pathname-defaults*) 'pathname)
			  (format t "~A" (frob-pathname *default-pathname-defaults*)))))
	    ;; Although the CLIM spec says the item formatter should try to fill
	    ;; the available width, I can't get either the item or table formatters
	    ;; to really do so such that the memory usage appears right justified.
	    ))))))


