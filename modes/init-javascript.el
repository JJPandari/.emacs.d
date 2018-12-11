(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js2-include-node-externs t
        js-indent-level 2
        js2-global-externs '("$" "jQuery" "jquery" "_")
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-bounce-indent-p nil)

  (general-define-key
   :states '(normal)
   :keymaps 'js2-mode-map
   "g d" 'js2-jump-to-definition)

  (add-hook! 'js2-mode-hook (setq mode-name "JS2")))

(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode))

;; (maybe-require-package 'typescript-mode)


(provide 'init-javascript)
