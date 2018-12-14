(jester/with-minor-leader 'with-editor-mode
                          "," 'with-editor-finish
                          "a" 'with-editor-cancel)

(use-package edit-server
  :hook (edit-server-start . visual-line-mode)
  :demand t
  :config
  (edit-server-start)
  (setq edit-server-new-frame nil
         edit-server-url-major-mode-alist
      '(("emacs-china\\.org" . markdown-mode)
        ("github\\.com" . markdown-mode)
        ("stackoverflow\\.com" . markdown-mode)))
  (jester/with-minor-leader 'edit-server-edit-mode
                            "," 'edit-server-done
                            "a" 'edit-server-abort))


(provide 'init-edit-server)
