;;----------------------------------------------------------------------------
;; highlight diff
;;----------------------------------------------------------------------------
(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

;;----------------------------------------------------------------------------
;; major modes for git files.
;;----------------------------------------------------------------------------
(use-package gitignore-mode
  :mode "\\.gitignore\\'")
(use-package gitconfig-mode
  :mode "\\.gitconfig\\'")

;;----------------------------------------------------------------------------
;; browse current file history
;;----------------------------------------------------------------------------
(use-package git-timemachine
  :init
  (jester/with-leader
   "g t" 'git-timemachine)
  :commands git-timemachine
  :config
  (add-hook! 'git-timemachine-mode-hook (evil-motion-state))
  (general-define-key
   :states 'motion
   :keymaps 'git-timemachine-mode-map
   "K" 'git-timemachine-show-previous-revision
   "J" 'git-timemachine-show-next-revision
   "q" 'git-timemachine-quit
   "s" 'git-timemachine-show-revision-fuzzy
   "r" 'git-timemachine-kill-abbreviated-revision))

;;----------------------------------------------------------------------------
;; The mighty magit
;;----------------------------------------------------------------------------
(use-package magit
  :init
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (jester/with-leader
   "g s" 'magit-status
   "g d" 'magit-diff-buffer-file
   "g r" 'diff-hl-revert-hunk
   ;; "a" ≈ "actions"
   "g a" 'magit-dispatch-popup)
  :commands (magit-status magit-diff-buffer-file magit-dispatch-popup)
  :config
  (jester/with-major-leader
   'magit-log-mode-map
   "a" 'magit-log-select-quit)
  )

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

;;----------------------------------------------------------------------------
;; github
;;----------------------------------------------------------------------------
;; TODO
;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t))

(provide 'init-git)
