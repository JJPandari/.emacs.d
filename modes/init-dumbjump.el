(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive t
        dumb-jump-selector 'ivy)
  (general-define-key
   :states '(normal motion)
   "g d" 'dumb-jump-go
   "g b" 'dumb-jump-back)
  (general-define-key
   :states '(normal motion)
   :keymaps '(emacs-lisp-mode-map lisp-mode-map)
   "g d" 'evil-goto-definition)
  :commands (dumb-jump-go))


(provide 'init-dumb-jump)
