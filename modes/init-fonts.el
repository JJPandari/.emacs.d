;;----------------------------------------------------------------------------
;; style texts a bit
;;----------------------------------------------------------------------------
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(setq display-line-numbers-width nil
      display-line-numbers-type 'relative
      display-line-numbers-current-absolute t)

(after-init (electric-pair-mode 1)) ;; this is global
(after-init (electric-indent-mode 1))
(after-init (global-prettify-symbols-mode 1))

(maybe-require-package 'list-unicode-display)

(global-hl-line-mode 1)
(add-hook 'prog-mode-hook (lambda () (which-function-mode 1)))

(use-package highlight-parentheses
  :init
  (setq hl-paren-colors '("Springgreen3" "firebrick1" "IndianRed1" "IndianRed3" "IndianRed4"))
  :demand t
  :config
  (global-highlight-parentheses-mode 1)
  (set-face-attribute 'hl-paren-face nil :weight 'bold))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1))

;;----------------------------------------------------------------------------
;; Set fonts.
;;----------------------------------------------------------------------------
(defun jester/use-small-font ()
  "Use 15px font."
  (interactive)
  (set-face-attribute 'default nil
                      ;; :family "Source Code Pro"
                      :family "Fira Code"
                      :height 150
                      :weight 'normal
                      :width 'normal)
  (jester/set-fallback-fonts))

(defun jester/use-large-font ()
  "Use 18px font."
  (interactive)
  (set-face-attribute 'default nil
                      ;; :family "Source Code Pro"
                      :family "Fira Code"
                      :height 180
                      :weight 'normal
                      :width 'normal)
  (jester/set-fallback-fonts))

(defun jester/set-fallback-fonts ()
  "Set fallback fonts for Chinese, some unicode chars..."
  (interactive)
  (when window-system
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family "Source Han Sans CN Regular")))
    (set-fontset-font t nil (font-spec :family "Fira Code") nil 'append)))

(jester/use-large-font)
;; try out Fira Code ligatures
(mac-auto-operator-composition-mode 1)

;;----------------------------------------------------------------------------
;; show trailing whitespace
;;----------------------------------------------------------------------------
(defun jester/show-trailing-whitespace ()
  (set-face-attribute 'trailing-whitespace nil
                      :background (face-attribute 'font-lock-comment-face :foreground))
  (setq show-trailing-whitespace 1))
(add-hook! 'prog-mode-hook (jester/show-trailing-whitespace))

;;----------------------------------------------------------------------------
;; adjust visual fill column
;;----------------------------------------------------------------------------
(defun jester/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'jester/maybe-adjust-visual-fill-column)


(provide 'init-fonts)
