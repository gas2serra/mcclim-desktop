(in-package :desktop-app-manager)

(clim:define-application-frame desktop-app-manager ()
  ((view-option :initform "menu"))
  (:menu-bar t)
  (:command-table (desktop-app-manager
		   :inherit-from (deski::edit-application-command-table)
		   :menu (("Quit" :command com-quit)
			  ("AppMan" :menu menubar-app-command-table)
			  ("Application" :menu deski::edit-application-command-table))))
  (:panes
   (application-display :application
			:height 300
			:width 400
			:display-function #'%update-application-display
			:display-time :command-loop)
   (interactor :interactor :display-time :command-loop)
   (edit-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-edit-option)
      (clim:radio-box-current-selection "yes")
      "no"))
   (view-option
    (clim:with-radio-box (:orientation :vertical
				       :value-changed-callback '%update-view-option)
      (clim:radio-box-current-selection "menu")
      "all")))
  (:layouts
   (default
    (clim:vertically ()
      (2/3
       (clim:horizontally nil
         (clim:labelling (:label "Applications")
           application-display)
         (clim:vertically nil
           (clim:labelling (:label "View")
             view-option)
           (clim:labelling (:label "Edit local files")
             edit-option)
	   clim:+fill+)))
      (1/3 (clim:labelling (:label "Interactor")
             interactor))))))

;; output

(defmethod clim:frame-standard-output ((frame desktop-app-manager))
  (clim:find-pane-named clim:*application-frame* 'application))

;; initialization

(defmethod clim:adopt-frame  :after (fm (frame desktop-app-manager))
  (declare (ignore fm)))

(defmethod clim:disown-frame  :after (fm (frame desktop-app-manager))
  (declare (ignore fm)))

;; commands

;; updating

(defun %update-edit-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (setf deski::*force-user-app-files-p*
	(string= (clim:gadget-label selected-gadget) "yes")))

(defun %update-view-option (this-gadget selected-gadget)
  (declare (ignore this-gadget))
  (with-slots (view-option) clim:*application-frame*
    (let ((label (clim:gadget-label selected-gadget)))
      (cond
	((string= label "menu")
	 (setf view-option "menu"))
	((string= label "all")
	 (setf view-option "all")))))
  (clim:redisplay-frame-pane clim:*application-frame*
			     (clim:find-pane-named clim:*application-frame* 'application-display)))
        
(defun %update-application-display (desktop-app-manager stream)
  (declare (ignore desktop-app-manager))
  (draw-app-table stream))

(clim:define-command-table menubar-app-command-table
    :menu (("Clear Interactor" :command (com-clear-interactor))
	   ("Refresh" :command (com-refresh))
	   ("Refresh Apps" :command (com-refresh-apps))
	   ("Quit" :command (com-quit))))


(defun draw-app-table (stream)
  (let ((max-width (round
		    (/ (/ (clim:rectangle-width
			   (clim:sheet-region stream))
			  2)
		       (clim:stream-string-width stream #\M)))))
    (with-slots (view-option) clim:*application-frame*
      (clim:with-drawing-options (stream :text-size :large)
	(if (string= view-option "all")
	    (format stream "~%   Registered Apps~%~%")
	    (format stream "~%   Menu Apps~%~%"))
	(fresh-line stream))
      (clim:formatting-table (stream :x-spacing '(2 :character))
	(clim:formatting-row (stream)
	  (clim:with-text-face (stream :italic)
	    (clim:formatting-cell (stream :align-x :center) (format stream "Name"))
	    (clim:formatting-cell (stream :align-x :center) (format stream "Prety Name"))
	    (clim:formatting-cell (stream :align-x :center) (format stream "Menu"))
	    (clim:formatting-cell (stream :align-x :center) (format stream "I/L/C"))
	    ))
	(dolist (app (deski::registered-applications))
	  (when (or (string= view-option "all") (application-menu-p app))
	    (clim:with-output-as-presentation (stream app 'application)
	      (clim:formatting-row (stream)
		(clim:formatting-cell (stream :align-x :left :align-y :top)
		  (princ (application-name app) stream))
		(clim:formatting-cell (stream :align-x :left :align-y :top)
		  (princ (application-pretty-name app) stream))
		(clim:formatting-cell (stream :align-x :left :align-y :top)
		  (princ (application-menu-p app) stream))
		(clim:formatting-cell (stream :align-x :left :align-y :top)
		  (when (typep app 'cl-application)
		    (format stream "~A/~A/~A"
			    (application-installed-p app)
			    (application-loaded-p app)
			    (application-configured-p app)))))))))
      (fresh-line stream))))
	      
	      

