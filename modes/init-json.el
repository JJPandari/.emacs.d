(use-package json-mode
  :mode "\\(?:\\(?:\\(?:\\.\\(?:b\\(?:\\(?:abel\\|ower\\)rc\\)\\|json\\(?:ld\\)?\\)\\|composer\\.lock\\)\\)\\'\\)"
  :config
  (jester/with-major-leader 'json-mode-map
    "f" 'json-mode-beautify))

(provide 'init-json)
