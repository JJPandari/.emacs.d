(use-package avy)

(use-package link-hint
  :after avy
  :commands link-hint-open-link
  :init
  (jester/with-leader
   "l o" 'link-hint-open-link)
  (general-define-key
   :states '(motion)
   "o" 'link-hint-open-link))

(provide 'init-avy)
