;;; SASS
(use-package sass-mode
  :mode "\\.sass\\'"
  :config
  (setq-default scss-compile-at-save nil))

;; css-mode & scss-mode & less-css-mode are built in
(setq css-indent-offset 2)


(provide 'init-css)