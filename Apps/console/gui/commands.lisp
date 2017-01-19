(in-package :desktop-console)


(define-desktop-console-command (com-quit :menu nil
				  :name "Quit"
				  :keystroke (#\q :meta))
    ()
  (clim:frame-exit clim:*application-frame*))

(define-desktop-console-command (com-clear-output :name "Clear output history"
						  :command-table application-commands
						  :provide-output-destination-keyword nil)
    ()
  (clim:window-clear *standard-output*))

(define-desktop-console-command (com-refresh  :menu nil
					      :name "Refresh"
					      :keystroke (#\r :meta))
    ()
  (refresh-applications)
  (update-applications))


(defvar *use-background-eval* nil)

(defun shuffle-specials (form values)
  (setf +++ ++
        ++  +
        +   form
        /// //
        //  /
        /   values
        *** **
        **  *
        *   (first values)))
(defun display-evalues (values)
  (labels
      ((present-value (value)         
         ;; I would really prefer this to behave as below, as presenting
         ;; things as expressions causes translators applicable to expression
         ;; to override those which would be otherwise applicable (such as
         ;; the set-current-package translator). I retain the use of w-o-a-p,
         ;; swapping the inner/outer presentation types, with the assumption
         ;; that someone (the form reader?) really does want expressions, and
         ;; the presentation-type-of is seldom a subtype of expression.
         ;; Aside from that, the problem with my code below is that it
         ;; will use the default presentation method for the type, which will
         ;; not necessarily print in the fashion expected from the lisp REPL.
         ;; Possibly this +listener-view+ could save the day here, but I'm
         ;; unclear on why it exists.   --Hefner

         ;; Okay, set-current-package translator now mysteriously works, but
         ;; I stand by the notion that 'expression should not be the type of
         ;; the innermost presentation.
         
         #+(or)
         (with-output-as-presentation (t value 'expression :single-box t)
           (present value (presentation-type-of value) :single-box t))

         (clim:with-output-as-presentation (t value (clim:presentation-type-of value)
                                         :single-box t)
           (clim:present value 'clim:expression))))
    (clim:with-drawing-options (t :ink clim:+olivedrab+)
      (cond ((null values) #+NIL (format t "No values.~%"))
            ((= 1 (length values))             
             (present-value (first values))
             (fresh-line))
            (t (do* ((i 0 (1+ i))
                     (items values (rest items))
                     (object (first items) (first items)))
                    ((null items))
               (clim:with-drawing-options (t :ink clim:+limegreen+)
                 (clim:with-text-style (t (clim:make-text-style nil :italic :small))
                   (format t "~A  " i)))
                 (present-value object)
                 (fresh-line)))))))

(define-desktop-console-command (com-eval :menu t)
    ((form 'clim:form :prompt "form"))
  (let ((standard-output *standard-output*)
        (standard-input *standard-input*)
        (debugger-hook *debugger-hook*)
	(application-frame clim:*application-frame*))
    (flet ((evaluate ()
             (let ((- form)
                   (*standard-output* standard-output)
                   (*standard-input* standard-input)
                   (*error-output* standard-output)
                   (*debugger-hook* debugger-hook)
		   (clim:*application-frame* application-frame)
                   error success)
               (if *use-background-eval*
                   (unwind-protect (handler-case (prog1 (cons :values (multiple-value-list (eval form)))
                                                   (setf success t))
                                     (serious-condition (e)
                                       (setf error e)
                                       (error e)))
                     (when (not success)
                       (return-from evaluate (cons :error error))))
                   (cons :values (multiple-value-list (eval form)))))))
      ;; If possible, use a thread for evaluation, permitting us to
      ;; interrupt it.
      (let ((start-time (get-internal-real-time)))
        (destructuring-bind (result . value)
            (if (and *use-background-eval* clim-sys:*multiprocessing-p*)
                (catch 'done
                  (let* ((orig-process (clim-sys:current-process))
                         (evaluating t)
                         (eval-process
                          (clim-sys:make-process
                           #'(lambda ()
                               (let ((result (evaluate)))
                                 (when evaluating
                                   (clim-sys:process-interrupt orig-process
                                                               #'(lambda ()
                                                                   (throw 'done result)))))))))
                    (unwind-protect
                         (handler-case (loop for gesture = (clim:read-gesture)
                                             when (and (typep gesture 'clim:keyboard-event)
                                                       (eq (clim:keyboard-event-key-name gesture) :pause))
                                             do (clim-sys:process-interrupt eval-process #'break))
                           (clim:abort-gesture ()
                             (clim-sys:destroy-process eval-process)
                             (cons :abort (/ (- (get-internal-real-time) start-time)
                                             internal-time-units-per-second))))
                      (setf evaluating nil))))
                (evaluate))
          (ecase result
            (:values
             (fresh-line)
             (shuffle-specials form value)
             (display-evalues value)
             (fresh-line))
            (:error (clim:with-text-style (t (clim:make-text-style nil :italic nil))
                      (if value
                          (clim:with-output-as-presentation (t value 'expression)
                            (format t "Aborted due to ~A: ~A" (type-of value) value))
                          (format t "Aborted for unknown reasons (possibly use of ~A)." 'break))))
            (:abort (clim:with-text-style (t (clim:make-text-style nil :italic nil))
                      (format t "Aborted by user after ~F seconds." value)))))))))
