(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  ;; the syntax table is totally sh*t
  (dolist (char '(?* ?+ ?- ?= ?/ ?< ?>))
    (modify-syntax-entry char "." coffee-mode-syntax-table))
  (modify-syntax-entry ?_ "w" coffee-mode-syntax-table)
  (jester/with-major-leader 'coffee-mode
                            "d" 'coffee-mark-defun))

(provide 'init-coffeescript)
