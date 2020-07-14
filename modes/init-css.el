;;; SASS
(use-package sass-mode
  :mode "\\.sass\\'"
  :config
  (setq-default scss-compile-at-save nil))

;; css-mode & scss-mode & less-css-mode are built in
(setq css-indent-offset 2)

(after-load 'css-mode
  (modify-syntax-entry ?. "." css-mode-syntax-table))

;; TODO need also consider when I'm on the ".class {" line
(defun jester/css-emmet-or-normal-tab ()
  "Do `emmet-expand-line' if current line doesn't have a \": \" yet, otherwise behave like normal tab."
  (interactive)
  (let ((has-divider))
    (save-excursion
      (move-beginning-of-line 1)
      (setq has-divider (search-forward ": " (line-end-position) t)))
    (if has-divider (jester/yas-or-company-or-hippie)
      (call-interactively 'emmet-expand-line))))
;; (general-define-key
;;  :states '(insert emacs)
;;  :keymaps '(css-mode-map sass-mode-map)
;;  "<tab>" 'jester/css-emmet-or-normal-tab)
;; TODO how to make a buffer-local company-active-map?
(add-hook! 'css-mode-hook
  ;; (make-local-variable 'company-active-map)
  ;; (general-define-key
  ;;  :keymaps 'company-active-map
  ;;  "<tab>" 'jester/css-emmet-or-normal-tab)
  ;; (setf (alist-get 'tab (cdr company-active-map)) 'jester/css-emmet-or-normal-tab)
  )

(provide 'init-css)
