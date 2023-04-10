(use-package hydra
  :custom (hydra-hint-display-type 'posframe)
  :demand t
  :config
  (plist-put hydra-posframe-show-params :poshandler 'posframe-poshandler-frame-bottom-center))


;; hydras are defined in other files grouped by their functionalities.


(provide 'init-hydra)
