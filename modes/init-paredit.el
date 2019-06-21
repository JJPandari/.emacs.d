
(push (expand-file-name "awesome-pair" jester-submodules-dir) load-path)
(use-package awesome-pair
  :ensure nil
  :commands (awesome-pair-kill)
  :bind (("M-u" . awesome-pair-wrap-round))
  :init
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
