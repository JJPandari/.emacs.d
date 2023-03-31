(use-package which-key
  :init
  ;; (jester/with-leader
  ;;  "k w" 'which-key-show-major-mode)
  :demand t
  :config
  (which-key-mode))

(use-package which-key-posframe
  :demand t
  :after which-key
  :config
  (which-key-posframe-mode))

(provide 'init-which-key)
