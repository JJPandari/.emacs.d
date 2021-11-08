(use-package php-mode
  :config
  (modify-syntax-entry ?$ "\_" php-mode-syntax-table))

;; (evil-define-key 'insert c-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
;; (evil-define-key 'insert php-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
;; (evil-define-key 'insert php-mode-map (kbd "C-j") (lambda () (interactive) (insert " => ")))

;; phps-mode


(provide 'init-php)
