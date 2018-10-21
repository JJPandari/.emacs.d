(require-package 'solarized-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'doom-themes)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(solarized-light))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;; after theme loaded https://emacs-china.org/t/topic/5994
;; (if (eq (frame-parameter frame 'background-mode) 'light)
;;     (add-to-list 'default-frame-alist '(ns-appearance . light))
;;   (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(provide 'init-themes)
