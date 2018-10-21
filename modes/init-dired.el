(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

;; (when (maybe-require-package 'diredfl)
;;   (after-load 'dired
;;     (diredfl-global-mode)))

;; (after-load 'dired
;;   (setq dired-recursive-deletes 'top)
;;   (define-key dired-mode-map [mouse-2] 'dired-find-file)
;;   (define-key dired-mode-map (kbd "C-c C-p") 'wdired-change-to-wdired-mode))

;; (when (maybe-require-package 'diff-hl)
;;   (after-load 'dired
;;     (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

(use-package ranger
  :init                                 ;; TODO define leader sequence
  :bind (([remap dired] . deer)
         :map ranger-mode-map
         ("s" . 'ranger-sort-criteria)))

(use-package all-the-icons
  :hook ranger-mode)

(use-package all-the-icons-dired
  :after all-the-icons)

(provide 'init-dired)
