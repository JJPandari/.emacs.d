(add-hook 'php-mode-hook (lambda () (progn
                                 (electric-indent-local-mode -1)
                                 (modify-syntax-entry ?$ "\_" php-mode-syntax-table))))

(evil-define-key 'insert c-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
(evil-define-key 'insert php-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
(evil-define-key 'insert php-mode-map (kbd "C-j") (lambda () (interactive) (insert " => ")))


(provide 'init-php)
