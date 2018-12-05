;; (require-package 'paredit)
;; (autoload 'enable-paredit-mode "paredit")

;; (defun maybe-map-paredit-newline ()
;;   (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
;;               (minibufferp))
;;     (local-set-key (kbd "RET") 'paredit-newline)))

;; (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)

;; (after-load 'paredit
;;   (diminish 'paredit-mode " Par")
;;   ;; Suppress certain paredit keybindings to avoid clashes, including
;;   ;; my global binding of M-?
;;   (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
;;     (define-key paredit-mode-map (read-kbd-macro binding) nil)))


;; ;; Compatibility with other modes

;; (suspend-mode-during-cua-rect-selection 'paredit-mode)


;; ;; Use paredit in the minibuffer
;; ;; TODO: break out into separate package
;; ;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

;; (defvar paredit-minibuffer-commands '(eval-expression
;;                                       pp-eval-expression
;;                                       eval-expression-with-eldoc
;;                                       ibuffer-do-eval
;;                                       ibuffer-do-view-and-eval)
;;   "Interactive commands for which paredit should be enabled in the minibuffer.")

;; (defun conditionally-enable-paredit-mode ()
;;   "Enable paredit during lisp-related minibuffer commands."
;;   (if (memq this-command paredit-minibuffer-commands)
;;       (enable-paredit-mode)))

;; ;; ----------------------------------------------------------------------------
;; ;; Enable some handy paredit functions in all prog modes
;; ;; ----------------------------------------------------------------------------

;; (require-package 'paredit-everywhere)
;; (after-load 'paredit-everywhere
;;   (define-key paredit-everywhere-mode-map (kbd "M-s") nil))
;; (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
;; (add-hook 'css-mode-hook 'paredit-everywhere-mode)



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
  :config
  (general-define-key :states '(normal operator) "L" 'jester/goto-end-of-sexp)
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

(provide 'init-paredit)
