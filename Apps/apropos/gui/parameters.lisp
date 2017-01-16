(in-package :desktop-apropos)

;;;
;;; navigator parameters
;;;

(defparameter *apropos-navigator-heading-text-style* (clim:make-text-style
						     nil
						     :bold 13))

(defparameter *apropos-navigator-sub-heading-text-style* (clim:make-text-style
						     nil
						     :bold 11))

(defparameter *apropos-navigator-subclas-of-options*
  (list (cons "nil" nil)
	(cons "sheet" 'clim:sheet)
	(cons "pane" 'clim:pane)
	(cons "gadget" 'clim:gadget)
	(cons "presentation" 'clim:presentation)
	(cons "command-table" 'clim:command-table)
	(cons "application-frame" 'clim:application-frame)))

(defparameter *apropos-navigator-metaclas-of-options*
  (list
   (cons "nil" nil)))
	
(defparameter *apropos-navigator-preselect-options*
  (list (cons "nil" nil)
	(cons "command"
	      #'command-internal-preselect)
	(cons "command-table"
	      #'command-table-preselect)
	(cons "presentation-type"
	      #'presentation-type-preselect)
	(cons "view-type"
	      #'view-type-preselect)))
	



