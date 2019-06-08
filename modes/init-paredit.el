;; https://emacs-china.org/t/paredit-smartparens/6727/11
(defun jester/paredit-space-for-delimiter-p (endp delm)
  (or (member 'font-lock-keyword-face (text-properties-at (1- (point))))
      (not (derived-mode-p 'basic-mode
                           'c++-mode
                           'c-mode
                           'coffee-mode
                           'csharp-mode
                           'd-mode
                           'dart-mode
                           'go-mode
                           'java-mode
                           'js-mode
                           'lua-mode
                           'objc-mode
                           'pascal-mode
                           'python-mode
                           'r-mode
                           'ruby-mode
                           'rust-mode
                           'typescript-mode
                           'web-mode
                           'css-mode))))

(use-package paredit
  :commands (paredit-kill paredit-raise-sexp paredit-wrap-sexp paredit-forward-slurp-sexp paredit-forward-barf-sexp)
  :bind (("M-r" . paredit-raise-sexp) ("M-u" . paredit-wrap-sexp))
  ;; :hook (prog-mode . paredit-mode)
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates 'jester/paredit-space-for-delimiter-p))


;; TODO make kbd "DL" delete whole lines when nothing before and only non-word after the kill end
(push (expand-file-name "awesome-pair" jester-submodules-dir) load-path)
(use-package awesome-pair
  :ensure nil
  :commands (awesome-pair-kill)
  :init
  (general-define-key
   :states '(normal)
   "d" (general-key-dispatch 'evil-delete
         "L" 'awesome-pair-kill)
   "c" (general-key-dispatch 'evil-change
         "L" (lambda () (interactive) (awesome-pair-kill) (evil-insert-state)))))

(provide 'init-paredit)
