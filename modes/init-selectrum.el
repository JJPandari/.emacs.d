(use-package selectrum
  :demand t
  :config
  (selectrum-mode 1))


(use-package prescient
  :custom (prescient-aggressive-file-save t)
  :demand t
  :after (selectrum)
  :config
  (prescient-persist-mode 1))

(use-package selectrum-prescient
  :demand t
  :after (selectrum prescient)
  :config
  (selectrum-prescient-mode 1))


(provide 'init-selectrum)
