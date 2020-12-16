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
 :states '(normal motion)
 :keymaps 'shell-mode-map
 "C-d" 'evil-scroll-down)

(add-hook! 'eshell-mode-hook
  (general-define-key
   :states '(normal motion)
   :keymaps 'eshell-mode-map
   "<return>" 'switch-to-buffer))

(defface jester-shell-face '((t :family "JetBrains Mono" :height 140))
  "Face for shell buffers. Use a different font and smaller font size.")

;; https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
(defun jester/set-shell-buffer-face ()
  "Set a different (smaller) face for a shell buffer."
  (buffer-face-set 'jester-shell-face))

(add-hook 'comint-mode-hook 'jester/set-shell-buffer-face)
(add-hook 'eshell-mode-hook 'jester/set-shell-buffer-face)
(add-hook 'term-mode-hook 'jester/set-shell-buffer-face)

(defun jester/shell-for-node.js ()
  "For buffer file, look up directories and find package.json, open a shell there."
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "package.json")))
    (eshell)))

;;----------------------------------------------------------------------------
;; eshell
;;----------------------------------------------------------------------------
(push (expand-file-name "aweshell" jester-submodules-dir) load-path)
(use-package aweshell
  :ensure nil
  :commands aweshell-new
  :config
  (with-eval-after-load "esh-opt"
    (setq eshell-prompt-function 'epe-theme-pipeline)))

;;----------------------------------------------------------------------------
;; ansi-term
;;----------------------------------------------------------------------------
(use-package term
  :ensure nil
  :init
  (jester/with-leader "t n" (lambda! (ansi-term "/bin/zsh")))
  :custom (term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  :config
  ;; unleash some keys
  (general-define-key
   :keymaps 'term-raw-map
   "M-:" nil
   "M-x" nil
   "C-h" nil
   "C-s" nil)
  ;; re-override keys overriden by evil
  (general-define-key
   :states '(insert emacs)
   :keymaps 'term-raw-map
   "<tab>" (lambda! (term-send-raw-string "\t"))
   "C-a" 'term-send-raw
   "C-e" 'term-send-raw
   "C-n" 'term-send-raw
   "C-p" 'term-send-raw
   "C-f" 'term-send-raw
   "C-d" 'term-send-raw
   "C-w" 'term-send-raw
   "C-k" 'term-send-raw
   "C-r" 'term-send-raw
   "C-t" 'term-send-raw
   "C-v" 'term-paste
   "C-/" (lambda! (term-send-raw-string "\C-_"))
   "C-c" 'term-interrupt-subjob
   "M-c" 'term-send-raw-meta
   "M-f" 'term-send-raw-meta
   "M-d" 'term-send-raw-meta
   "M-b" 'term-send-raw-meta
   "<escape>" (lambda! (term-line-mode) (evil-motion-state)))
  (general-define-key
   :states '(normal motion)
   :keymaps '(term-raw-map term-mode-map)
   "<return>" 'switch-to-buffer)
  (general-define-key
   :states '(normal motion)
   :keymaps 'term-mode-map
   "a" (lambda! (evil-insert-state) (term-char-mode))))


(provide 'init-shell)
