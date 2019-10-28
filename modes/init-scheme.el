;; (setq scheme-program-name "csi -:c") ; for `run-scheme'
(setq scheme-program-name "racket -i") ; for `run-scheme'

(add-hook! 'racket-mode-hook
  (setq compile-command (concat "racket -f " (shell-quote-argument (buffer-file-name))))
  (setq jester-run-command (concat "racket -f " (shell-quote-argument (buffer-file-name)))))

(use-package racket-mode
  :mode "\\.\\(rkt[dl]?\\)\\|\\(scm\\)\\'"
  :config
  (jester/with-major-leader 'racket-mode-map
    "i" 'run-scheme))


(provide 'init-scheme)
