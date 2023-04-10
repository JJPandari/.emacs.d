(setq shell-file-name "/bin/bash"
      explicit-shell-file-name "/bin/zsh")

(general-define-key
 :states '(insert emacs)
 :keymaps 'comint-mode-map
 "C-d" 'backward-char
 "C-b" 'comint-delchar-or-maybe-eof
 "C-r" 'comint-history-isearch-backward
 "C-l" 'comint-clear-buffer)

(general-define-key
 :states '(insert emacs)
 :keymaps 'shell-mode-map
 "C-r" 'counsel-shell-history)

(defface jester-shell-face '((t :family "JetBrains Mono" :height 140))
  "Face for shell buffers. Use a different font and smaller font size.")

;; https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
(defun jester/set-shell-buffer-face ()
  "Set a different (smaller) face for a shell buffer."
  (buffer-face-set 'jester-shell-face))

(add-hook 'comint-mode-hook 'jester/set-shell-buffer-face)
(add-hook 'vterm-mode-hook 'jester/set-shell-buffer-face)

(defun jester/shell-for-node.js ()
  "For buffer file, look up directories and find package.json, open a shell there."
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "package.json")))
    (vterm)))

;;----------------------------------------------------------------------------
;; vterm
;;----------------------------------------------------------------------------
(use-package vterm
  :custom ((vterm-shell "/bin/zsh")
           (vterm-kill-buffer-on-exit t))
  :init
  (jester/with-leader "t n" (lambda! (vterm 'new-session)))
  :hook (vterm-mode . evil-insert-state)
  :config
  ;; vterm-mode-map won't work because evil-insert-state-map takes precedence
  ;; copy them to insert-state|vterm-mode map
  (dolist (key '("<tab>"
                 "C-f" "C-d" "C-a" "C-e" "C-n" "C-p"
                 "C-b" "C-w" "C-k"
                 "C-r" "C-t" "C-/"
                 "M-c" "M-f" "M-d" "M-b"))
    (general-define-key
     :states '(insert emacs)
     :keymaps 'vterm-mode-map
     key (lookup-key vterm-mode-map (kbd key))))

  (general-define-key
   :keymaps 'vterm-mode-map
   "M-:" 'eval-expression)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'vterm-mode-map
   "C-h" nil
   "C-c" 'vterm--self-insert
   "C-v" 'vterm-yank
   "C-s" 'swiper)

  (general-define-key
   :states '(normal motion)
   :keymaps 'vterm-mode-map
   "<return>" 'switch-to-buffer
   "i" (lambda! (vterm-reset-cursor-point) (evil-insert-state))
   "a" (lambda! (vterm-reset-cursor-point) (evil-insert-state))))


(provide 'init-shell)
