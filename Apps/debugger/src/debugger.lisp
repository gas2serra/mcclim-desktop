#| CLIM Debugger

TODO
----------------------------------------
- Elliott Johnson is to be thanked for the nice scroll-bars, but
  for some reason they don't remember their position when clicking
  on a stack-frame or "more".

- The break function does not use the clim-debugger --> Christophe
  Rhodes was kind enough to inform me that on SBCL,
  SB-EXT:*INVOKE-DEBUGGER-HOOK* takes care off this problem. I
  still don't know if this is a problem with other compilers.

- "Eval in frame" is not supported. I don't know of a good way to
   do this currently.

- Goto source location is not supported, but I think this could be
  done through slime.

- Currently the restart chosen by the clim-debugger is returned
  through the global variable *returned-restart*, this is not the
  best solution, but I do not know how of a better way to return a
  value from a clim frame, when it exits.

- There need to added keyboard shortcuts. 'q' should exit the
  debugger with an abort. '0', '1' and so forth should activate
  the restarts, like Slime. Maybe is should be possible to use the
  arrow keys as well. Then we have to add a notion of the current
  frame. Would this be useful?

|#

(in-package desktop-debugger)

(defmacro bold ((stream) &body body)
  `(clim:with-text-face (,stream :bold)
     ,@body))




;;; CLIM stuff
;;; ----------------------------------------

(defclass debugger-pane (clim:application-pane)
  ((condition-info :reader condition-info :initarg :condition-info)))

(defmethod condition-info (p)
  deski::*condition*)

;; FIXME - These two variables should be removed!
;; Used to return the chosen reatart in the debugger.
(defparameter *returned-restart* nil)



(defun make-debugger-pane ()
  (clim:with-look-and-feel-realization ((clim:frame-manager clim:*application-frame*)
					clim:*application-frame*) 
    (clim:make-pane 'debugger-pane 
		    :condition-info deski::*condition*
		    :display-function #'display-debugger
		    :end-of-line-action :allow
		    :end-of-page-action :scroll)))

(clim:define-application-frame clim-debugger (;;esa::esa-frame-mixin
					      clim:standard-application-frame)
  ;;() 
  ;;(:esa-gui t :presentation-history? t)
  ()
  (:pointer-documentation t)
  (:panes (debugger-pane (make-debugger-pane))
          (interactor :interactor))
  (:layouts (:default (clim:vertically () 
                        (clim:scrolling () debugger-pane)
                        (250 interactor))))
  (:geometry :height 600 :width 800))

(defun run-debugger-frame ()
  (clim:run-frame-top-level
   (clim:make-application-frame 'clim-debugger)))

(clim:define-presentation-type pr-stack-frame () :inherit-from 'deski::stack-frame)
(clim:define-presentation-type more-type   ())
(clim:define-presentation-type inspect     ())

(define-clim-debugger-command (com-more :name "More backtraces")
    ((pane 'more-type :default (clim:find-pane-named clim:*application-frame* 'debugger-pane)))
  (deski::expand-backtrace (condition-info pane) 10))

(define-clim-debugger-command (com-invoke-inspector :name "Invoke inspector")
    ((obj 'inspect))
  (clouseau:inspector obj))

(define-clim-debugger-command (com-refresh :name "Refresh" :menu t) ()
  (clim:change-space-requirements (clim:frame-panes clim:*application-frame*)))

(define-clim-debugger-command (com-quit :name "Quit" :menu t) ()
  (clim:frame-exit clim:*application-frame*))

(define-clim-debugger-command (com-invoke-restart :name "Invoke restart")
    ((restart 'deski::restart))
  (setf *returned-restart* restart)
  (clim:frame-exit clim:*application-frame*))

(define-clim-debugger-command (com-toggle-stack-frame-view 
			       :name "Toggle stack frame view")
    ((stack-frame 'deski::stack-frame))
  (progn
    (if (eq deski::+minimized-stack-frame-view+ (deski::view stack-frame))
	(setf (deski::view stack-frame) deski::+maximized-stack-frame-view+)
	(setf (deski::view stack-frame) deski::+minimized-stack-frame-view+))
    (clim:change-space-requirements (clim:frame-panes clim:*application-frame*))))

(clim:define-presentation-to-command-translator more-backtraces
    (more-type com-more clim-debugger :gesture :select)
    (object)
  (list object))

(clim:define-presentation-to-command-translator invoke-inspector
    (inspect com-invoke-inspector clim-debugger :gesture :select)
    (object)
  (list object))

(clim:define-presentation-to-command-translator toggle-stack-frame-view
    (pr-stack-frame com-toggle-stack-frame-view clim-debugger :gesture :select)
    (object)
  (list object))

(clim:define-presentation-to-command-translator invoke-restart
    (restart com-invoke-restart clim-debugger :gesture :select)
    (object)
  (list object))

(defun std-form (pane first second &key (family :sans-serif))
  (clim:formatting-row 
      (pane)
    (clim:with-text-family (pane :sans-serif)
      (clim:formatting-cell (pane) (bold (pane) (format t "~A" first))))
    (clim:formatting-cell (pane)
      (clim:with-text-family (pane family) 
	(format t "~A" second)))))

(defun display-debugger (frame pane)
  (let ((*standard-output* pane))
    (clim:formatting-table (pane)
      (std-form pane "Condition type:" (deski::type-of-condition (condition-info
							   pane)))
      (std-form pane "Description:"    (deski::condition-message (condition-info
							   pane)))
      (when (deski::condition-extra (condition-info pane))
        (std-form pane "Extra:" (deski::condition-extra (condition-info pane))
                  :family :fix)))
    (fresh-line)
    
    (clim:with-text-family (pane :sans-serif)
      (bold (pane) (format t "Restarts:")))
    (fresh-line)
    (format t " ")
    (clim:formatting-table 
	(pane)
      (loop
	 for r in (deski::restarts (condition-info pane))
	 for i from 0 
	 do (clim:formatting-row (pane)
	      (clim:with-output-as-presentation (pane r 'deski::restart :single-box t)
		(clim:formatting-cell (pane)
		  (bold (pane) (format pane "~A: " i)))
		(clim:formatting-cell (pane)
		  (clim:with-drawing-options (pane :ink clim:+deep-pink+)
		    (format pane "[~A]" (restart-name r))))
		(clim:formatting-cell (pane)
		  (clim:with-text-family (pane :sans-serif)
		    (format pane "~A" r)))))))
    (fresh-line)
    (display-backtrace frame pane)
    (clim:change-space-requirements pane
                  :width (clim:bounding-rectangle-width (clim:stream-output-history pane))
                  :height (clim:bounding-rectangle-height (clim:stream-output-history pane)))))

(defun display-backtrace (frame pane)
  (declare (ignore frame)) 
  (clim:with-text-family (pane :sans-serif)
    (bold (pane) (format t "Backtrace:")))
  (fresh-line)
  (format t " ")
  (clim:formatting-table 
      (pane)
    (loop for stack-frame in (deski::backtrace (condition-info pane))
       for i from 0
       do (clim:formatting-row (pane)
	    (clim:with-output-as-presentation (pane stack-frame 'deski::stack-frame)
	      (bold (pane) (clim:formatting-cell (pane) (format t "~A: " i)))
	      (clim:formatting-cell (pane)
		(clim:present stack-frame 'pr-stack-frame 
			 :view (deski::view stack-frame))))))
    (when (>= (length (deski::backtrace (condition-info pane))) 20)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane))
        (clim:formatting-cell (pane)
          (bold (pane)
            (clim:present pane 'more-type)))))))

(clim:define-presentation-method clim:present (object (type pr-stack-frame) stream
					    (view deski::minimized-stack-frame-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((str (deski::frame-string object)))
    (format t "~A  "
	    (if (> (length str) 300)
		(subseq str 0 300)
		str))))

(clim:define-presentation-method clim:present (object (type pr-stack-frame) stream
                     (view deski::maximized-stack-frame-view)
                     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (progn
    (princ (deski::frame-string object) stream)
    (fresh-line)
    (clim:with-text-family (stream :sans-serif)
      (bold (stream) (format t "  Locals:")))
    (fresh-line)
    (format t "     ")
    (clim:formatting-table 
	(stream)
      (loop for (name n identifier id value val) in (deski::frame-variables object)
	 do (clim:formatting-row 
		(stream)
	      (clim:formatting-cell (stream) (format t "~A" n))
	      (clim:formatting-cell (stream) (format t "="))
	      (clim:formatting-cell (stream) (clim:present val 'inspect)))))
    (fresh-line)))
	  
(clim:define-presentation-method clim:present (object (type more-type) stream
					    (view clim:textual-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (bold (stream) (format t "--- MORE ---")))

(clim:define-presentation-method clim:present (object (type inspect) stream
					    (view clim:textual-view)
					    &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (format t "~A" object))

;;; Starting the debugger
;;; ----------------------------------------

(defun debugger (condition me-or-my-encapsulation)
  (swank-backend::call-with-debugging-environment 
   (lambda ()
     (unwind-protect
	  (progn 
	    (setf 
	     deski::*condition* 
	     (deski::make-debugger-info condition))
	    (run-debugger-frame))
       (let ((restart *returned-restart*))
	 (setf *returned-restart* nil)
	 (setf deski::*condition* nil)
	 (if restart
	     (let ((*debugger-hook* me-or-my-encapsulation))
	       (invoke-restart-interactively restart))
	     (abort)))))))
