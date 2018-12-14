(use-package avy
  :init
  (setq avy-all-windows nil)
  (jester/with-leader "j j" 'avy-goto-char-timer)
  :commands (avy-goto-char-timer))

(use-package link-hint
  :after avy
  :commands link-hint-open-link
  :init
  (jester/with-leader
   "l o" 'link-hint-open-link)
  (general-define-key
   :states '(motion)
   :keymaps '(help-mode-map Man-mode-map)
   "f" 'link-hint-open-link))

(provide 'init-avy)
