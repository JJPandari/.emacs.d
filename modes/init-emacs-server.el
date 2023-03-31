;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
;; "unless running" is to avoid prompting error when starting a second Emacs
;; when app opened as main editor, start default server
(unless (server-running-p)
  (server-start))
;; when opened as maid, start server with name "maid"
(when (string= (getenv "EMACS_SOCKET") "maid")
  (unless (server-running-p "maid")
    (setq server-name "maid")
    (setq frame-title-format "maid")
    (setq desktop-base-file-name ".emacs.maid.desktop"
          desktop-base-lock-name ".emacs.maid.desktop.lock")
    (server-start)))

(provide 'init-emacs-server)
