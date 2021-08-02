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
  :config
  (global-highlight-parentheses-mode 1)
  ;; run after init due to `reapply-themes'
  ;; TODO load-theme-hook
  (add-hook! :append 'after-init-hook
    ;; the theme's setting will take precedence if simply `setq'
    (customize-set-variable 'hl-paren-colors '("Springgreen3" "firebrick1" "IndianRed1" "IndianRed3" "IndianRed4")))
  (set-face-attribute 'hl-paren-face nil :weight 'bold))
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1))

(use-package all-the-icons
  :demand t)

;;----------------------------------------------------------------------------
;; highlight region with symbol-overlay
;;----------------------------------------------------------------------------
(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode)
         (css-mode . symbol-overlay-mode)
         (yaml-mode . symbol-overlay-mode)
         (conf-mode . symbol-overlay-mode)
         (markdown-mode . symbol-overlay-mode)
         (help-mode . symbol-overlay-mode))
  :init
  ;; don't put temporary highlight
  (setq symbol-overlay-idle-time 0)
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps '(prog-mode-map css-mode yaml-mode conf-mode markdown-mode help-mode)
   "<tab>" 'jester/probably-symbol-overlay-put
   "H-n" 'symbol-overlay-jump-next
   "H-p" 'symbol-overlay-jump-prev)
  (jester/with-leader "o p" 'jester/probably-symbol-overlay-put)
  ;; don't bind any key
  (setq symbol-overlay-map (make-sparse-keymap))

  ;; TODO off when region active. maybe PR?
  (defun jester/probably-symbol-overlay-put ()
    "Probably highlight region or symbol at point with symbol-overlay, but:
toggle file when peeking definitions with lsp-ui."
    (interactive)
    (if (and (featurep 'lsp-ui) lsp-ui-peek-mode)
        (call-interactively 'lsp-ui-peek--toggle-file)
      (if (region-active-p)
          (progn
            (symbol-overlay-put-all
             (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))
             symbol-overlay-scope)
            (deactivate-mark))
        (symbol-overlay-put)))))

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

;;----------------------------------------------------------------------------
;; face related key bindings
;;----------------------------------------------------------------------------
(jester/with-leader
 "t c" 'text-scale-adjust)


(provide 'init-face)
