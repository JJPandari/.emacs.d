(general-define-key
 :keymaps '(minibuffer-local-map minibuffer-local-ns-map minibuffer-local-completion-map minibuffer-local-must-match-map minibuffer-local-isearch-map
                                 evil-ex-map evil-ex-completion-map)
 "M-d" #'backward-word
 "M-b" #'kill-word
 "C-w" #'backward-kill-word
 "C-d" #'backward-char
 "C-b" #'delete-char
 "C-v" #'yank
 "C-S-k" 'jester/kill-back-to-indentation
 "<escape>" 'keyboard-escape-quit)

(general-define-key
 :keymaps 'read-expression-map
 "C-r" 'counsel-minibuffer-history)

(provide 'init-minibuffer)
