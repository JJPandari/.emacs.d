(use-package avy
  :init
  (setq avy-all-windows t)
  ;; (jester/with-leader "j j" 'avy-goto-char-timer)
  :commands (avy-goto-char-timer))

(use-package link-hint
  :after avy
  :commands link-hint-open-link
  :init
  (jester/with-leader
   "l o" 'link-hint-open-link))

(provide 'init-avy)
