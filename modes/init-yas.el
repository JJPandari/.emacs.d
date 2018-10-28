(use-package yasnippet
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

  (jester/with-major-leader 'snippet-mode
   "," 'yas-load-snippet-buffer-and-close
   "l" 'yas-load-snippet-buffer)

  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory))))

(provide 'init-yas)
