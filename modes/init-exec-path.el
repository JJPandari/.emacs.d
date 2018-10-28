(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  :config
  (dolist (var '("GOPATH" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var)))

(provide 'init-exec-path)
