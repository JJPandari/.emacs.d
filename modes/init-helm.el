(with-eval-after-load 'helm
  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-buffers)
  (define-key helm-map (kbd "C-w") 'backward-kill-word))


(provide 'init-helm)
