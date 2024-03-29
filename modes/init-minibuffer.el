(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(general-define-key
 :keymaps '(minibuffer-local-map minibuffer-local-ns-map minibuffer-local-completion-map minibuffer-local-must-match-map minibuffer-local-isearch-map
                                 evil-ex-map evil-ex-completion-map)
 "M-d" #'backward-word
 "M-b" #'kill-word
 "C-w" #'backward-kill-word
 "C-f" #'forward-char
 "C-d" #'backward-char
 "S-<backspace>" #'delete-char
 "C-v" #'yank
 "C-y" #'evil-paste-from-register
 "C-S-k" 'jester/kill-back-to-indentation
 "H-r" 'counsel-minibuffer-history
 "H-x" 'kill-region
 "<escape>" 'keyboard-escape-quit)

(general-define-key
 :keymaps 'evil-ex-completion-map
 "M-p" 'previous-complete-history-element
 "M-n" 'next-complete-history-element)


;; (use-package mini-frame
;;   :demand t
;;   :config
;;   (mini-frame-mode 1))

(provide 'init-minibuffer)
