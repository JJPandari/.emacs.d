(after-init (electric-pair-mode 1)) ;; this is global

(push (expand-file-name "awesome-pair" jester-submodules-dir) load-path)
(use-package awesome-pair
  :ensure nil
  :commands (awesome-pair-kill
             awesome-pair-open-round awesome-pair-open-bracket awesome-pair-open-curly
             awesome-pair-close-round awesome-pair-close-bracket awesome-pair-close-curly)
  :bind (("M-u" . awesome-pair-wrap-round))
  :init
  (general-define-key :states '(insert emacs) "(" 'awesome-pair-open-round)
  (general-define-key :states '(insert emacs) "[" 'awesome-pair-open-bracket)
  (general-define-key :states '(insert emacs) "{" 'awesome-pair-open-curly)
  (general-define-key :states '(insert emacs) ")" 'awesome-pair-close-round)
  (general-define-key :states '(insert emacs) "]" 'awesome-pair-close-bracket)
  (general-define-key :states '(insert emacs) "}" 'awesome-pair-close-curly)

  (defun jester/semantic-kill-maybe-whole-line ()
    "Kill semantic unit after point, if only whitespace is left afterwards, delete this line."
    (interactive)
    (awesome-pair-kill)
    (when (and (looking-back "^ *") (looking-at " *$"))
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
