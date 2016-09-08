(in-package :mcclim-desktop-launcher)

(define-launcher-frame-command com-launch-app ((app 'application))
  (launch-application app))

(define-launcher-frame-command com-refresh-application ((app 'application))
  (refresh-application *manager* app))

(define-launcher-frame-command com-configure-application ((app 'application))
  (configure-application app t))

(define-launcher-frame-command com-edit-application-file ((app 'application))
  (when (application-file app)
    (edit-file (application-file app))))

(define-launcher-frame-command com-edit-config-file ((app 'application))
  (when (application-config-file app)
    (edit-file (application-config-file app))))

(define-launcher-frame-command com-open-home-page ((app 'application))
  (launch-application (find-application "browser") :args (list (application-home-page app))))

;;
;; command traslators
;;

(clim:define-presentation-to-command-translator launch-app
    (application com-launch-app launcher-frame
		 :gesture :select
		 :documentation "Launch")
    (object) (list object))

(clim:define-presentation-to-command-translator refresh-application
    (application com-refresh-application launcher-frame
		       :gesture :help
		       :documentation "Refresh")
  (object) (list object))

(clim:define-presentation-to-command-translator configure-application
    (application com-configure-application launcher-frame
		 :gesture :help
		 :documentation "Configure")
  (object) (list object))


(clim:define-presentation-to-command-translator edit-application-file
    (application com-edit-application-file launcher-frame
		 :gesture :help
		 :documentation "Edit application file")
    (object) (list object))

(clim:define-presentation-to-command-translator edit-config-file
    (application com-edit-config-file launcher-frame
		 :gesture :help
		 :documentation "Edit config file")
    (object) (list object))

(clim:define-presentation-to-command-translator open-home-page
    (application com-open-home-page launcher-frame
		 :gesture :help
		 :documentation "Open home page")
    (object) (list object))
