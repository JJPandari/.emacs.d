(use-package markdown-mode
  :init (add-hook! 'markdown-mode-hook
          (setq-local electric-pair-pairs (append '((?` . ?`)) electric-pair-pairs))
          (setq truncate-lines nil))
  :commands markdown-mode
  :mode "\\.md\\.html\\'"
  :config
  (set-face-attribute 'markdown-code-face nil
                      :family "Fira Code"
                      :height 140))


(provide 'init-markdown)
