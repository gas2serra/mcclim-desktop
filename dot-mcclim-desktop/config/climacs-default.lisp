(in-package :desktop-user)

;;;
;;; clipboard
;;;

(in-package drei)

(define-command (com-paste-x-clipboard :name t :command-table drei:editing-table) ()
  (flexichain:insert-sequence (point) (desktop-sys:paste-from-x11-clipboard)))

(define-command (com-copy-x-clipboard :name t :command-table drei:view-table) ()
  (desktop-sys:copy-to-x11-clipboard (coerce (kill-ring-yank *kill-ring*) 'string)))

(esa-io::set-key 'com-paste-x-clipboard 'drei:editing-table '((#\y :META :CONTROL)))
(esa-io::set-key 'com-copy-x-clipboard 'drei:view-table '((#\w :META :CONTROL)))
