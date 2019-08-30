(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(jester/with-major-leader 'ranger-mode-map
                          "w" 'wdired-change-to-wdired-mode)

(jester/with-major-leader 'wdired-mode-map
                          "," 'wdired-finish-edit
                          "a" 'wdired-abort-changes)

(use-package ranger
  :init
  ;; NOTE: remap doesn't change when dired-* is called from code
  (jester/with-leader
   "f d" 'dired-jump)
  :bind (([remap dired-jump] . deer)
         :map ranger-mode-map
         ("s" . 'ranger-sort-criteria))
  :config
  (ranger-override-dired-mode 1))

(use-package all-the-icons-dired
  :after (all-the-icons ranger)
  :hook ((dired-mode . all-the-icons-dired-mode)
         (ranger-mode . all-the-icons-dired-mode)))

(provide 'init-dired)
