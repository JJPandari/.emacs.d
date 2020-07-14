(use-package restclient
  :init
  (defun jester/restclient ()
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (switch-to-buffer (current-buffer))))
  :mode ("\\.rest\\'" . restclient-mode)
  :hook (restclient-mode . display-line-numbers-mode)
  :config
  (general-define-key
   :keymaps 'restclient-mode-map
   "C-c C-c" (lambda! (restclient-http-send-current nil t)))
  (general-define-key
   :states '(normal visual)
   :keymaps 'restclient-mode-map
   "<tab>" 'restclient-toggle-body-visibility)
  (general-define-key
   :states '(insert emacs)
   :keymaps 'restclient-mode-map
   "C-j" 'jester/make-simple-assignment))


(provide 'init-http)
