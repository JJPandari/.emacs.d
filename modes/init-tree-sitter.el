(setq treesit-language-source-alist '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
                                      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                                      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                                      (python . ("https://github.com/tree-sitter/tree-sitter-python"))
                                      (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))))

;; M-x treesit-install-language-grammar


(provide 'init-tree-sitter)
