(use-package yasnippet
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :demand t
  :config
  (yas-global-mode 1)

  (general-define-key
   :keymaps 'yas-keymap
   "TAB" nil
   "<tab>" nil
   "<return>" #'yas-next-field
   "C-g" 'abort-company-or-yas
   "C-d" nil
   "C-b" 'yas-skip-and-clear-or-delete-char)

  (jester/with-leader
   "s v" 'yas-visit-snippet-file
   "s n" 'yas-new-snippet)

  (jester/with-major-leader 'snippet-mode-map
    "l" 'yas-load-snippet-buffer)

  (defun jester/yas-expand-no-prompt ()
    "Like `yas-expand', but unlike `yas-expand', who prompts when same snippet exist in parent and child mode, this function just use the one in child mode."
    (interactive)
    (let ((yas-prompt-functions '(yas-no-prompt)))
      (yas-expand))))

(provide 'init-yas)
