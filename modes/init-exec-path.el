;; only do exec path init once
(defvar cache-path-from-shell-loaded-p nil)
(advice-add 'exec-path-from-shell-initialize :around
            (lambda (oldfun &rest args)
              (if (and cache-path-from-shell-loaded-p (not (called-interactively-p)))
                  (message "exec-path-from-shell done already.")
                (setq cache-path-from-shell-loaded-p t)
                (apply oldfun args))))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  :config
  (dolist (var '("SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "SSH_AUTH_SOCK"
                 "GOPATH" "MVN_HOME" "JAVA_HOME"))
    (push var exec-path-from-shell-variables)))

(provide 'init-exec-path)
