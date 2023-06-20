;;----------------------------------------------------------------------------
;; style texts a bit
;;----------------------------------------------------------------------------
(add-hook 'text-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
(add-hook 'conf-mode-hook (lambda () (display-line-numbers-mode 1)))
(setq display-line-numbers-width nil
      display-line-numbers-type 'relative
      display-line-numbers-current-absolute t)

(after-init (global-prettify-symbols-mode 1))

(maybe-require-package 'list-unicode-display)

(global-hl-line-mode 1)

(use-package highlight-parentheses
  :demand t
  :init
  (customize-set-variable 'highlight-parentheses-colors
                          '("springgreen3" "IndianRed1" "IndianRed3" "IndianRed4"
                            "#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
  :config
  (global-highlight-parentheses-mode 1)
  (add-hook! 'load-theme-hook
    ;; the theme's setting will take precedence if simply `setq'
    (customize-set-variable 'highlight-parentheses-colors
                            '("springgreen3" "IndianRed1" "IndianRed3" "IndianRed4"
                              "#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
  (set-face-attribute 'hl-paren-face nil :weight 'bold))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1))

(use-package all-the-icons
  :demand t)
;; (use-package nerd-icons
;;   :demand t)

;;----------------------------------------------------------------------------
;; Set fonts.
;;----------------------------------------------------------------------------
(defvar jester-small-font-size
  (pcase window-system
    ('mac 150)
    ('w32 140)
    ('x 140)
    ('nil 140))
  "small font size depending on system.")
(defvar jester-large-font-size
  (pcase window-system
    ('mac 160)
    ('w32 150)
    ('x 150)
    ('nil 140))
  "large font size depending on system.")

(defun jester/use-small-font ()
  "Use (relatively) small font."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      ;; :family "JetBrains Mono"
                      ;; :family "Fira Code"
                      :height jester-small-font-size
                      :weight 'normal
                      :width 'normal)
  (jester/set-fallback-fonts))

(defun jester/use-large-font ()
  "Use (relatively) large font."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      ;; :family "JetBrains Mono"
                      ;; :family "Fira Code"
                      :height jester-large-font-size
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
;; try out ligatures
(when *is-a-mac* (mac-auto-operator-composition-mode 1))

;;----------------------------------------------------------------------------
;; show trailing whitespace
;;----------------------------------------------------------------------------
(defun jester/show-trailing-whitespace ()
  (set-face-attribute 'trailing-whitespace nil
                      :background (face-attribute 'font-lock-comment-face :foreground))
  (setq show-trailing-whitespace 1))
(add-hook! 'prog-mode-hook 'jester/show-trailing-whitespace)

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


(provide 'init-face)
