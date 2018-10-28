;;----------------------------------------------------------------------------
;; Nicer naming of buffers for files with identical names
;;----------------------------------------------------------------------------
(require 'uniquify)

(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*")


(provide 'init-uniquify)
