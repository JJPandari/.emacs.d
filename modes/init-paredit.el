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
   "H-w" (lambda! (awesome-pair-wrap-round-pair)))

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


(use-package puni
  :init
  ;; (general-define-key
  ;;  :states '(insert emacs)
  ;;  :keymaps '(prog-mode-map)
  ;;  ;; delete commands
  ;;  [remap backward-delete-char] 'puni-backward-delete-char
  ;;  [remap backward-delete-char-untabify] 'puni-backward-delete-char
  ;;  [remap delete-char] 'puni-forward-delete-char
  ;;  [remap delete-forward-char] 'puni-forward-delete-char
  ;;  [remap kill-word] 'puni-forward-kill-word
  ;;  [remap backward-kill-word] 'puni-backward-kill-word
  ;;  [remap evil-delete-backward-word] 'puni-backward-kill-word
  ;;  [remap kill-line] 'puni-kill-line
  ;;  [remap jester/kill-back-to-indentation] 'puni-backward-kill-line
  ;;  ;; (define-key map (kbd "C-c DEL") 'puni-force-delete)
  ;;  [remap kill-region] 'puni-kill-active-region
  ;;  ;; navigation commands
  ;;  [remap forward-sexp] 'puni-forward-sexp
  ;;  [remap backward-sexp] 'puni-backward-sexp
  ;;  [remap beginning-of-sexp] 'puni-beginning-of-sexp
  ;;  [remap end-of-sexp] 'puni-end-of-sexp)
  (general-define-key
   :states '(normal)
   :keymaps 'prog-mode-map
   ;; manipulating commands
   ">" 'puni-slurp-forward
   "<" 'puni-barf-forward
   "M->" 'puni-slurp-backward
   "M-<" 'puni-barf-backward))


(provide 'init-paredit)
