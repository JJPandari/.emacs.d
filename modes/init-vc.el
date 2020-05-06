(use-package smerge-mode
  :ensure nil
  :init
  (jester/with-leader
   "m u" 'smerge-keep-upper
   "m l" 'smerge-keep-lower
   "m b" 'smerge-keep-all
   "m n" 'smerge-next))

(provide 'init-vc)
