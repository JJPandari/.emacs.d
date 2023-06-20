(require-package 'solarized-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)
(require-package 'base16-theme)
(require-package 'zenburn-theme)

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-scale-outline-headlines nil)

(load-theme (if (and (boundp 'server-name) (string= server-name "maid"))
                'doom-tomorrow-night
              'solarized-light)
            t)


;; https://gist.github.com/hlissner/1ace77658c772cf150a43dc9396fa2ed
(defvar load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme'.")

(defun run-load-theme-hooks (theme &optional _no-confirm no-enable)
  "Set up `load-theme-hook' to run after `load-theme' is called."
  (unless no-enable
    (run-hooks 'load-theme-hook)))

(advice-add #'load-theme :after #'run-load-theme-hooks)

;; TODO `enable-theme-functions' when 29
(add-hook! 'load-theme-hook
  (when (memq 'solarized-light custom-enabled-themes)
    (custom-theme-set-faces
     'solarized-light
     '(web-mode-keyword-face ((t (:inherit 'font-lock-keyword-face)))))))


(provide 'init-themes)
