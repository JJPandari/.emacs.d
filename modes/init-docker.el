(use-package docker
  :init
  ;; (jester/with-leader "d d" 'docker)
  :commands docker
  :config
  (fullframe docker-images tablist-quit)
  (fullframe docker-machines tablist-quit)
  (fullframe docker-volumes tablist-quit)
  (fullframe docker-networks tablist-quit)
  (fullframe docker-containers tablist-quit)
  ;; TODO copy emacs state binding to motion state
  (evil-set-initial-state 'docker-image-mode 'emacs)
  (evil-set-initial-state 'docker-machine-mode 'emacs)
  (evil-set-initial-state 'docker-volume-mode 'emacs)
  (evil-set-initial-state 'docker-network-mode 'emacs)
  (evil-set-initial-state 'docker-container-mode 'emacs))

(use-package dockerfile-mode
  ;; TODO auto upper-case first word on line?
  :init
  (add-hook! 'dockerfile-mode-hook (setq tab-width 0))
  :mode "\\Dockerfile\\'")

(use-package docker-compose-mode)


(provide 'init-docker)
