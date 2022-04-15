(defvar cucumber-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\@ "_" table)
    table))

(defvar cucumber-mode-font-lock-keywords
  (list
   (cons (rx (and "@" (+ (or (syntax word) (syntax symbol))))) 'font-lock-function-name-face)
   (cons (rx (or "Feature"
                 "Scenario"
                 "Given"
                 "When"
                 "Then"
                 "And")) 'font-lock-keyword-face))
  "Font lock for cucumber mode.")

(define-derived-mode cucumber-mode prog-mode "🥒"
  "Major mode for cucumber .feature files"
  :syntax-table cucumber-mode-syntax-table
  (setq font-lock-defaults '(cucumber-mode-font-lock-keywords)))

(add-auto-mode 'cucumber-mode "\\.feature\\'")


(provide 'init-cucumber)
