(use-package rust-mode
  ;; :custom (rust-indent-offset 2)
  :init
  (add-hook! 'rust-mode-hook
    (setq compile-command "cargo build")
    (setq jester-run-command "cargo run"))
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :init
  (add-hook! 'flycheck-mode-hook #'flycheck-rust-setup))


(jester/def-make-assignment-function
 jester/make-rust-assignment
 ";"
 ;; [empty] <=> let <=> let mut
 (cond
  ((looking-at "let mut ") (replace-match ""))
  ((looking-at "let ") (replace-match "let mut "))
  (t (insert "let "))))

(defun jester/rust-insert-fat-or-slim-arrow ()
  "Insert a fat or slim arrow, depending on whether in a match body."
  (interactive)
  (save-match-data
    (insert (if (save-excursion
                  (search-backward "{")
                  (beginning-of-line-text)
                  (looking-at "match"))
                " => "
              " -> "))))

(general-define-key
 :states '(insert emacs)
 :keymaps 'rust-mode-map
 "C-j" 'jester/make-rust-assignment
 "C-l" 'jester/rust-insert-fat-or-slim-arrow)

(provide 'init-rust)
