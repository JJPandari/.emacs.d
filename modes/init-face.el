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
  ;; the theme's setting will take precedence if simply `setq'
  (customize-set-variable 'hl-paren-colors '("Springgreen3" "firebrick1" "IndianRed1" "IndianRed3" "IndianRed4"))
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

(use-package all-the-icons
  :demand t)

;;----------------------------------------------------------------------------
;; highlight region with hi-lock
;;----------------------------------------------------------------------------
;; (general-define-key
;;  :states '(normal visual motion)
;;  :keymaps '(prog-mode-map markdown-mode-map)
;;  "<tab>" 'jester/toggle-highlight-at-point)
;; (jester/with-leader
;;  "h i" 'hi-lock-find-patterns
;;  "h l" 'highlight-lines-matching-regexp
;;  "h p" 'highlight-phrase
;;  "h r" 'highlight-regexp
;;  "h s" 'highlight-symbol-at-point
;;  "h u" 'unhighlight-regexp
;;  "h b" 'hi-lock-write-interactive-patterns)

;; (defun jester/toggle-highlight-at-point ()
;;   "Toggle highlight at point (region or symbol)."
;;   (interactive)
;;   (require 'hi-lock)
;;   (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns))
;;         (hi-regexp-at-pt (jester/regexp-at-point))
;;         (hi-lock-auto-select-face t))
;;     (if (member hi-regexp-at-pt hi-regexp-list)
;;         (unhighlight-regexp hi-regexp-at-pt)
;;       (highlight-phrase hi-regexp-at-pt (hi-lock-read-face-name)))
;;     (deactivate-mark)))

;; (defun jester/clear-all-hi-lock ()
;;   "clear all highlight."
;;   (interactive)
;;   (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns)))
;;     (mapcar 'unhighlight-regexp hi-regexp-list)))

;; (defun jester/regexp-at-point ()
;;   "if region active, return the region,
;; otherwise return regexp like \"\\\\_<sym\\\\_>\" for the symbol at point."
;;   (if (region-active-p)
;;        (buffer-substring-no-properties
;;         (region-beginning) (region-end))
;;      (format "\\_<%s\\_>" (thing-at-point 'symbol t))))

;;----------------------------------------------------------------------------
;; highlight region with symbol-overlay
;;----------------------------------------------------------------------------
(use-package symbol-overlay
  :hook ((prog-mode . symbol-overlay-mode)
         (css-mode . symbol-overlay-mode)
         (yaml-mode . symbol-overlay-mode)
         (conf-mode . symbol-overlay-mode)
         (markdown-mode . symbol-overlay-mode))
  :init
  ;; don't put temporary highlight
  (setq symbol-overlay-idle-time 0)
  :config
  (general-define-key
   :states '(normal visual motion)
   :keymaps '(prog-mode-map)
   "<tab>" 'jester/symbol-overlay-put
   ;; "<C-i>" 'symbol-overlay-put
   "M-n" 'symbol-overlay-jump-next
   "M-p" 'symbol-overlay-jump-prev)
  ;; don't bind any key
  (setq symbol-overlay-map (make-sparse-keymap))

  ;; TODO off when region active. maybe PR?
  (defun jester/symbol-overlay-put ()
    "Highlight region or symbol at point with symbol-overlay."
    (interactive)
    (if (region-active-p)
        (progn
          (symbol-overlay-put-all
           (buffer-substring-no-properties (region-beginning) (region-end))
           symbol-overlay-scope)
          (deactivate-mark))
      (symbol-overlay-put))))

;;----------------------------------------------------------------------------
;; Set fonts.
;;----------------------------------------------------------------------------
(defun jester/use-small-font ()
  "Use 15px font."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      ;; :family "Fira Code"
                      :height 150
                      :weight 'normal
                      :width 'normal)
  (jester/set-fallback-fonts))

(defun jester/use-large-font ()
  "Use 18px font."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      ;; :family "Fira Code"
                      :height 160
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


(provide 'init-face)
