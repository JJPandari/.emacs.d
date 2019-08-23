(use-package nginx-mode
  :custom (nginx-indent-level 2)
  :mode ("/nginx/.+\\.conf\\'" "nginx\\.conf\\'"))

(provide 'init-nginx)
