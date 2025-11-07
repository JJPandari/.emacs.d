(use-package avy
  :init
  (setq avy-all-windows nil)
  ;; (general-define-key
  ;;  :states '(normal visual motion)
  ;;  "g a" 'avy-goto-char-timer)
  ;; (jester/with-leader "'" 'evil-avy-goto-char-timer)
  )

(use-package link-hint
  :after avy
  :init
  (jester/with-leader
   "l o" 'link-hint-open-link
   "l <return>" 'link-hint-open-link-at-point))

(provide 'init-avy)
