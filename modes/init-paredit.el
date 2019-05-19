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
  :init
  (general-define-key :states '(normal operator) "L" 'jester/goto-end-of-sexp)
  :commands (paredit-kill paredit-raise-sexp paredit-wrap-sexp paredit-forward-slurp-sexp paredit-forward-barf-sexp jester/goto-end-of-sexp)
  :bind (("M-r" . paredit-raise-sexp) ("M-u" . paredit-wrap-sexp))
  :hook (prog-mode . paredit-mode)
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates 'jester/paredit-space-for-delimiter-p)
  ;; TODO make kbd "DL" delete whole lines when nothing before and only non-word after the kill end

  (defun jester/goto-kill-end (kill-fun forward?)
    "When supplied with a kill function `kill-fun', go to the point the kill function should kill to."
    (let* ((old-max (point-max))
           (new-max (progn (funcall kill-fun) (point-max)))
           (kill-end (progn
                       (undo-tree-undo)
                       (funcall (if forward? '+ '-) (point) (abs (- new-max old-max))))))
      (goto-char kill-end)))

  (defun jester/goto-end-of-sexp ()
    "Go to the end of current expression."
    (interactive)
    (jester/goto-kill-end 'paredit-kill t)))


;; (add-to-list 'load-path "<path-to-awesome-pair>") ; add awesome-pair to your load-path
;; (require 'awesome-pair)

(provide 'init-paredit)
