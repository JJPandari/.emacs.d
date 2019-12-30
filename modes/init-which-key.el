(use-package which-key
  :init
  (jester/with-leader
   "k w" 'which-key-show-major-mode)
  :demand t
  :config
  (which-key-mode))

(provide 'init-which-key)
