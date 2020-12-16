(use-package elm-mode
  :custom (elm-indent-offset 2)
  :config
  (general-define-key
   :states '(insert emacs)
   :keymaps 'elm-mode-map
   "C-j" 'elm-indent-insert-equal))

(use-package flycheck-elm
  :after elm-mode
  :demand t
  :config
  (flycheck-elm-setup))

(provide 'init-elm)
