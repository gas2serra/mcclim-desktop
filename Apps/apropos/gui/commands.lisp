(in-package :desktop-apropos)

;;;
;;; command tables
;;;

(clim:make-command-table 'edit-menu)

;;;
;;; commands
;;;

(define-apropos-navigator-command (com-quit :menu t
					    :name "Quit"
					    :keystroke (#\q :meta))
    ()
  (%update-return-values)
  (clim:frame-exit clim:*application-frame*))

;;; edit

(clim:define-command (com-edit-select-all :command-table edit-menu
					  :menu t
					  :name "Select All"
					  :keystroke (#\a :meta :control))
    ()
  (with-slots (selected-values iapropos) clim:*application-frame*
    (setf selected-values (iapropos-matching-symbols iapropos)))
  (%maybe-update-output-display))

(clim:define-command (com-edit-select-none :command-table edit-menu
					   :menu t
					   :name "Select None"
					   :keystroke (#\n :meta :control))
    ()
  (with-slots (selected-values iapropos) clim:*application-frame*
    (setf selected-values nil))
  (%maybe-update-output-display))

(clim:define-command (com-edit-copy-to-kill-ring :command-table edit-menu
						 :menu t
						 :name "Copy To Kill Ring"
						 :keystroke (#\k :meta :control))
    ()
  (setf (clim:port-keyboard-input-focus (clim:port clim:*application-frame*)) 
	(car (clim:sheet-children
	      (clim:find-pane-named clim:*application-frame* 'symbol-regex-text-field))))
  (%update-return-values)
  (desktop-sys:copy-to-kill-ring (format nil "~A" *return-values*)))

(clim:define-command (com-edit-copy-to-clipboard :command-table edit-menu
						 :menu t
						 :name "Copy To Clipboard"
						 :keystroke (#\c :meta :control))
    ()
  (%update-return-values)
  (desktop-sys:copy-to-x11-clipboard (format nil "~S" *return-values*)))
  

;;; keystroke

(clim:define-command (com-edit-move-focus :command-table edit-menu
						 :menu nil
						 :keystroke (#\Tab))
    ()
  (let ((sym-sheet (car (clim:sheet-children
			 (clim:find-pane-named clim:*application-frame* 'symbol-regex-text-field))))
	(pac-sheet (car (clim:sheet-children
			 (clim:find-pane-named clim:*application-frame* 'package-regex-text-field)))))
    (setf (clim:port-keyboard-input-focus (clim:port clim:*application-frame*))
	      (if (eq
		   (clim:port-keyboard-input-focus (clim:port clim:*application-frame*))
		   sym-sheet)
		  pac-sheet
		  sym-sheet))))

;;; gesture :select

(define-apropos-navigator-command (com-select-symbol :name "Select Symbol")
    ((sym 'symbol :gesture :select))
  (with-slots (selected-values selected-action-option) clim:*application-frame*
    (if (eq selected-action-option :single)
	(setf selected-values (list sym))
	(if (member sym selected-values)
	    (setf selected-values (remove sym selected-values))
	    (setf selected-values (remove-duplicates (push sym selected-values))))))
  (with-fixed-vertical-scroll-bar (clim:find-pane-named
				   clim:*application-frame* 'symbol-result-display)
    (%maybe-update-symbol-result-display))
  (%maybe-update-output-display))

(define-apropos-navigator-command (com-select-package
				   :name "Select Package")
    ((pack 'package :gesture :select))
  (setf (clim:gadget-value
	 (clim:find-pane-named clim:*application-frame* 'package-regex-text-field))
	(format nil "^~A$" (package-name pack))))

(define-apropos-navigator-command (com-inspect-object
				   :name "Inspect Object")
    ((object 'object :gesture :select))
  (clouseau:inspector object))

(define-apropos-navigator-command (com-edit-definition :name "Edit Definition")
    ((loc 'source-location :gesture :select))
  (climacs:edit-file (car loc))
  (unless (climacs::find-climacs-frame)
    (sleep 1))
  (clim:execute-frame-command (climacs::find-climacs-frame)
			      (list 'drei-commands::com-goto-position (cdr loc))))
