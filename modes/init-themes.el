(require-package 'solarized-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)
(require-package 'base16-theme)
(require-package 'zenburn-theme)

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-scale-outline-headlines nil)

;; If you don't customize it, this is the theme you get.
(setq custom-enabled-themes (if (string= (getenv "EMACS_SOCKET") "maid")
                                '(doom-tomorrow-night)
                              '(solarized-light)))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;; https://gist.github.com/hlissner/1ace77658c772cf150a43dc9396fa2ed
(defvar load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme'.")

(defun run-load-theme-hooks (theme &optional _no-confirm no-enable)
  "Set up `load-theme-hook' to run after `load-theme' is called."
  (unless no-enable
    (run-hooks 'load-theme-hook)))

(advice-add #'load-theme :after #'run-load-theme-hooks)

;; TODO not work
(add-hook! 'load-theme-hook
  (when (memq 'solarized-dark-high-contrast custom-enabled-themes)
    ;; (custom-theme-set-faces
    ;;  'solarized-dark-high-contrast
    ;;  '(default ((t (:foreground "#9AABAC")))))
    ))

(add-hook! 'load-theme-hook
  (when (memq 'solarized-light custom-enabled-themes)
    (custom-theme-set-faces
     'solarized-light
     '(web-mode-keyword-face ((t (:inherit 'font-lock-keyword-face)))))))


(provide 'init-themes)
