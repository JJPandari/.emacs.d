(setq scheme-program-name "csi -:c")
(spacemacs/set-leader-keys "oe" (lambda () (interactive) (shell-command (concat "csi " buffer-file-name))))

(provide 'init-scheme)
