(use-package eyebrowse
  :demand t
  :config
  (eyebrowse-mode 1)
  (jester/with-leader
   "l l" #'eyebrowse-switch-to-window-config
   "l p" #'eyebrowse-prev-window-config
   "l n" #'eyebrowse-next-window-config
   "l TAB" #'eyebrowse-last-window-config
   "l k" #'eyebrowse-close-window-config
   "l r" #'eyebrowse-rename-window-config
   "l 0" #'eyebrowse-switch-to-window-config-0
   "l 1" #'eyebrowse-switch-to-window-config-1
   "l 2" #'eyebrowse-switch-to-window-config-2
   "l 3" #'eyebrowse-switch-to-window-config-3
   "l 4" #'eyebrowse-switch-to-window-config-4
   "l 5" #'eyebrowse-switch-to-window-config-5
   "l 6" #'eyebrowse-switch-to-window-config-6
   "l 7" #'eyebrowse-switch-to-window-config-7
   "l 8" #'eyebrowse-switch-to-window-config-8
   "l 9" #'eyebrowse-switch-to-window-config-9))

(provide 'init-eyebrowse)
