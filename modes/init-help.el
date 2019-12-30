(general-define-key
 :states 'motion
 :keymaps '(help-mode-map Man-mode-map)
 "K" 'help-go-back
 "J" 'help-go-forward
 "f" 'link-hint-open-link)

(general-define-key
 :states 'motion
 :keymaps 'Info-mode-map
 "K" 'Info-history-back
 "J" 'Info-history-forward
 "d" 'evil-scroll-down
 "u" 'evil-scroll-up
 "D" 'Info-directory
 "U" 'Info-up
 "H" 'Info-prev
 "L" 'Info-next
 "C-r" 'Info-history
 "f" 'link-hint-open-link
 "g g" 'evil-goto-first-line)


(provide 'init-help)
