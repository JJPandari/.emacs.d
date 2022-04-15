(use-package json-mode
  :init
  (add-hook! 'json-mode-hook (aggressive-indent-mode -1))
  :mode "\\(?:\\(?:\\(?:\\.\\(?:b\\(?:\\(?:abel\\|ower\\)rc\\)\\|json\\(?:ld\\)?\\)\\|composer\\.lock\\)\\)\\'\\)"
  :config
  (jester/with-major-leader 'json-mode-map
    "f" 'json-mode-beautify))

(provide 'init-json)
