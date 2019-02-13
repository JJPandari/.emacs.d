(use-package eglot
  :commands (eglot eglot-ensure))


;; not needed when using eglot
;; (use-package company-lsp
;;   :after company
;;   :init
;;   (add-hook! 'company-mode-hook (require 'company-lsp))
;;   :config
;;   (push 'company-lsp company-backends))
;; (use-package lsp-vue
;;   :demand t)

(provide 'init-lsp)
