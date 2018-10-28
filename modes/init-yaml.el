(when (maybe-require-package 'yaml-mode)
  (add-auto-mode 'yaml-mode "\\.ya?ml\\'")
  ;; make buttons for urls in comments/strings
  (add-hook 'yaml-mode-hook 'goto-address-prog-mode))


(provide 'init-yaml)
