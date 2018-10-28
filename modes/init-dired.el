(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(jester/with-major-leader 'Deer:name
 "w" 'wdired-change-to-wdired-mode)

(use-package ranger
  ;; TODO into deer by invoking dired not showing icons?
  :init
  (jester/with-leader
   "f d" 'dired-jump)
  :bind (([remap dired-jump] . deer)
         :map ranger-mode-map
         ("s" . 'ranger-sort-criteria)))

(use-package all-the-icons
  :after ranger
  :commands all-the-icons-insert)

;; TODO icon showing?
(use-package all-the-icons-dired
  :after (all-the-icons ranger)
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-dired)
