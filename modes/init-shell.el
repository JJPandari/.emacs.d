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

(defface jester-shell-face '((t :family "Fira Code" :height 140))
  "Face for shell buffers. Use a different font and smaller font size.")

;; https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
(defun jester/set-shell-buffer-face ()
  "Set a different (smaller) face for a shell buffer."
  (buffer-face-set 'jester-shell-face))

(add-hook 'comint-mode-hook 'jester/set-shell-buffer-face)
(add-hook 'eshell-mode-hook 'jester/set-shell-buffer-face)

(defun jester/shell-for-node.js ()
  "For buffer file, look up directories and find package.json, open a shell there."
  (interactive)
  (let ((default-directory (locate-dominating-file (buffer-file-name) "package.json")))
    (eshell)))


(push (expand-file-name "aweshell" jester-submodules-dir) load-path)
(use-package aweshell
  :ensure nil
  :commands aweshell-new
  :config
  (with-eval-after-load "esh-opt"
    (setq eshell-prompt-function 'epe-theme-pipeline)))


(provide 'init-shell)
