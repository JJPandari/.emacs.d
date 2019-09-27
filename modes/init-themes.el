(require-package 'solarized-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)
(require-package 'zenburn-theme)

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil
      solarized-scale-outline-headlines nil)

;; If you don't customize it, this is the theme you get.
(setq custom-enabled-themes '(solarized-light))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(provide 'init-themes)
