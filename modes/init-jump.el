(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive t
        dumb-jump-selector 'ivy)
  (general-define-key
   :states '(normal motion)
   "g d" 'dumb-jump-go
   ;; "g b" 'dumb-jump-back
   "g b" 'xref-pop-marker-stack)
  :commands (dumb-jump-go))

(general-define-key
 :states '(normal motion)
 :keymaps '(emacs-lisp-mode-map lisp-mode-map)
 "g d" 'evil-goto-definition)
(general-define-key
 :definer 'minor-mode
 :states 'normal
 :keymaps 'lsp-mode
 "g d" 'lsp-ui-peek-find-definitions
 "g r" 'lsp-ui-peek-find-references)

(provide 'init-jump)
