(use-package hydra
  :demand t
  :after (:any git-timemachine ibuffer)
  :config

  (defhydra hydra-git-timemachine (nil nil :foreign-keys run)
    "git timemachine"
    ("K" git-timemachine-show-previous-revision "earlier")
    ("J" git-timemachine-show-next-revision "later")
    ("q" git-timemachine-quit "quit" :exit t)
    ("s" git-timemachine-show-revision-fuzzy "arbitrary jump")
    ("r" git-timemachine-kill-abbreviated-revision "copy rev"))
  (add-hook! 'git-timemachine-mode-hook (hydra-git-timemachine/body)))

(provide 'init-hydra)
