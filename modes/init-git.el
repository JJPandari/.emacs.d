;;----------------------------------------------------------------------------
;; highlight diff
;;----------------------------------------------------------------------------
(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (jester/with-leader
   "d n" 'diff-hl-next-hunk
   "d p" 'diff-hl-previous-hunk))

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
  (add-hook! 'git-timemachine-mode-hook (evil-motion-state 1))
  (general-define-key
   :states 'motion
   :keymaps 'git-timemachine-mode-map
   "K" 'git-timemachine-show-previous-revision
   "J" 'git-timemachine-show-next-revision
   "q" 'git-timemachine-quit
   "s" 'git-timemachine-show-revision-fuzzy
   "r" 'git-timemachine-kill-abbreviated-revision)

  ;; FIXME: hydra occupies the keys even when ivy is up
  ;; (defhydra jester/git-timemachine-hydra (nil nil :foreign-keys run)
  ;;   "git timemachine"
  ;;   ("K" git-timemachine-show-previous-revision "earlier")
  ;;   ("J" git-timemachine-show-next-revision "later")
  ;;   ("q" git-timemachine-quit "quit" :exit t)
  ;;   ("s" git-timemachine-show-revision-fuzzy "arbitrary jump")
  ;;   ("r" git-timemachine-kill-abbreviated-revision "copy rev"))
  ;; (add-hook! 'git-timemachine-mode-hook (jester/git-timemachine-hydra/body))
  )

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
   "g l" 'magit-log
   "g f" 'magit-find-file
   ;; "a" ≈ "actions"
   "g a" 'magit-dispatch-popup)
  :commands (magit-status magit-diff-buffer-file magit-dispatch-popup)
  :config
  ;; quit rev selection in a rebase or something
  (jester/with-major-leader
      'magit-log-mode-map
    "a" 'magit-log-select-quit)
  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-one))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

;; (use-package transient
;;   :custom (transient-enable-popup-navigation t))

;;----------------------------------------------------------------------------
;; github & gitlab etc.
;;----------------------------------------------------------------------------
;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t))

;; (use-package forge
;;   :after magit)

(provide 'init-git)
