;; parens are taken care by paredit etc., others shall be electric.
(after-init (electric-pair-mode 1)) ;; this is global

(push (expand-file-name "awesome-pair" jester-submodules-dir) load-path)
(use-package awesome-pair
  :ensure nil
  :commands (awesome-pair-kill
             awesome-pair-backward-delete awesome-pair-forward-delete
             awesome-pair-equal awesome-pair-double-quote awesome-pair-space
             awesome-pair-open-round awesome-pair-open-bracket awesome-pair-open-curly
             awesome-pair-close-round awesome-pair-close-bracket awesome-pair-close-curly
             awesome-pair-wrap-round-pair)
  :init
  (general-define-key
   :states '(insert emacs)
   :keymaps 'prog-mode-map
   ;; "(" 'awesome-pair-open-round
   ;; "[" 'awesome-pair-open-bracket
   ;; "{" 'awesome-pair-open-curly
   ;; ")" 'awesome-pair-close-round
   ;; "]" 'awesome-pair-close-bracket
   ;; "}" 'awesome-pair-close-curly
   "=" 'awesome-pair-equal
   ;; "\"" 'awesome-pair-double-quote
   "SPC" 'awesome-pair-space
   ;; "<backspace>" 'awesome-pair-backward-delete
   ;; "C-b" 'awesome-pair-forward-delete
   )
  (general-define-key
   :states '(emacs insert normal visual)
   "M-u" (lambda! (awesome-pair-wrap-round-pair)))

  ;; TODO append \n to make it evil style
  (defun jester/semantic-kill-maybe-whole-line ()
    "Kill semantic unit after point, if only whitespace is left afterwards, delete this line."
    (interactive)
    (awesome-pair-kill)
    (when (and (looking-back "^\s*") (looking-at "\s*$"))
      (delete-region (point) (line-end-position 0))))

  (general-define-key
   :states '(normal)
   "d" (general-key-dispatch 'evil-delete
         "L" 'jester/semantic-kill-maybe-whole-line)
   "c" (general-key-dispatch 'evil-change
         "L" (lambda! (awesome-pair-kill) (evil-insert-state))))
  ;; binding to normal state implicitly binds to visual state, gotta fix it.
  (general-define-key
   :states '(visual)
   "d" 'evil-delete
   "c" 'evil-change))

(provide 'init-paredit)
