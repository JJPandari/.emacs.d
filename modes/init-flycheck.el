(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-idle-change-delay 1)
  (evil-set-initial-state 'flycheck-error-list-mode 'motion)
  (jester/with-leader
   "e l" 'flycheck-list-errors
   "e p" 'flycheck-previous-error
   "e n" 'flycheck-next-error)
  (general-define-key
   :states '(motion)
   :keymaps 'flycheck-error-list-mode-map
   "j" 'flycheck-error-list-next-error
   "k" 'flycheck-error-list-previous-error
   "f" 'flycheck-error-list-set-filter
   "F" 'flycheck-error-list-reset-filter
   "r" 'flycheck-error-list-check-source
   "e" 'flycheck-error-list-explain-error
   "<return>" 'flycheck-error-list-goto-error))

(use-package flycheck-posframe
  :if window-system
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(provide 'init-flycheck)
