(require-package 'solarized-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)
(require-package 'base16-theme)
(require-package 'zenburn-theme)

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-scale-outline-headlines nil)

(defun jester/customize-theme-a-bit (theme-name)
  "Customize themes a bit after theme loaded."
  (when (eq theme-name 'solarized-light)
    (custom-set-faces '(web-mode-keyword-face ((t (:inherit font-lock-keyword-face
                                                            :foreground unspecified
                                                            :weight unspecified)))))))

(add-hook 'enable-theme-functions 'jester/customize-theme-a-bit)

(load-theme (if (and (boundp 'server-name) (string= server-name "maid"))
                'doom-tomorrow-night
              'solarized-light)
            t)


(provide 'init-themes)
