;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 case-fold-search t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 truncate-lines t
 tab-width 2)
(delete-selection-mode 1)
(blink-cursor-mode -1)

(after-init (global-auto-revert-mode 1))
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      initial-major-mode 'emacs-lisp-mode
      kill-ring-max 200
      recentf-max-saved-items 1000)

(require 'recentf)
(add-to-list 'recentf-exclude (list "/tmp/" "/ssh:" "COMMIT_EDITMSG\\'"
                                   (recentf-expand-file-name package-user-dir)))

(after-init (transient-mark-mode 1))

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode))


;; (use-package symbol-overlay
;;   :hook ((prog-mode . symbol-overlay-mode)
;;          (css-mode . symbol-overlay-mode)
;;          (yaml-mode . symbol-overlay-mode)
;;          (conf-mode . symbol-overlay-mode)
;;          (markdown-mode . symbol-overlay-mode))
;;   :init
;;   ;; don't put temporary highlight
;;   (setq symbol-overlay-idle-time 0)
;;   :config
;;   (general-define-key
;;    :states '(normal motion)
;;    :keymaps '(prog-mode-map markdown-mode-map)
;;    "<tab>" 'symbol-overlay-put
;;    ;; "<C-i>" 'symbol-overlay-put
;;    "M-n" 'symbol-overlay-jump-next
;;    "M-p" 'symbol-overlay-jump-prev)
;;   ;; don't bind any key
;;   (setq symbol-overlay-map (make-sparse-keymap)))


(use-package expand-region
  :commands er/expand-region
  :init
  (general-define-key
   :states '(insert emacs)
   "<C-backspace>" 'er/expand-region))


(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))


(use-package string-inflection)

;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(defun jester/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(general-define-key
 :states '(insert emacs)
 "C-S-w" 'jester/kill-back-to-indentation)

;;----------------------------------------------------------------------------
;; insert or remove char at eol.
;;----------------------------------------------------------------------------
;; https://emacs-china.org/t/topic/4494/25?u=jjpandari
(defun insert-or-remove-trailing-char (c)
  (let ((fn (lambda (c)
              (end-of-line)
              (if (eq (char-before) c)
                  (delete-backward-char 1)
                (insert-char c)))))
    (save-excursion
      (if (region-active-p)
          (save-restriction
            (narrow-to-region (region-beginning) (region-end))
            (deactivate-mark)
            (goto-char (point-min))
            (funcall fn c)
            (while (< (point) (point-max))
              (next-line)
              (when (< (point) (point-max))
                (funcall fn c))))
        (funcall fn c)))))

(defun insert-or-remove-trailing-semi (&optional arg)
  (interactive "*P")
  (insert-or-remove-trailing-char ?\;))

(defun insert-or-remove-trailing-comma (&optional arg)
  (interactive "*P")
  (insert-or-remove-trailing-char ?,))

(general-define-key
 "C-;" 'insert-or-remove-trailing-semi
 "C-," 'insert-or-remove-trailing-comma)

;;----------------------------------------------------------------------------
;; Move lines up and down.
;;----------------------------------------------------------------------------
;; (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
;; (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))
(defun jester/move-line-up (count)
  "Move `prefix-arg' lines up."
  (interactive "p")
  (move-text-up
   (if (region-active-p) (region-beginning) nil)
   (if (region-active-p) (region-end) nil)
   count)
  (indent-for-tab-command))

(defun jester/move-line-down (count)
  "Move `prefix-arg' lines down."
  (interactive "p")
  (move-text-down
   (if (region-active-p) (region-beginning) nil)
   (if (region-active-p) (region-end) nil)
   count)
  (indent-for-tab-command))

(general-define-key
 :states '(normal)
 "z k" 'jester/move-line-up
 "z j" #'jester/move-line-down)

;;----------------------------------------------------------------------------
;; like web-mode-on-post-command
;;----------------------------------------------------------------------------
(defun jester/on-post-newline ()
  "Insert an extra line if inside a tag."
  (let (n)
    (when (and (member this-command '(newline electric-newline-and-maybe-indent newline-and-indent))
               (and (looking-back ">\n[\s\t]*" (point-min))
                    (not (looking-back "/[^>]*>\n[\s\t]*" (point-min)))
                    (looking-at "[\s\t]*</")
                    ))
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode))))

;;----------------------------------------------------------------------------
;; Make ternary expressions.
;;----------------------------------------------------------------------------
(defun jester/expand-to-ternary ()
  "Add a ternary expression, using the symbol before point as the first field."
  (interactive)
  (yas-expand-snippet " ? $1 : $0"))

(defun jester/expand-to-ternary-condensed ()
  "Add a (condensed) ternary expression, using the symbol before point as the first field."
  (interactive)
  (yas-expand-snippet "?$1:$0"))

(general-define-key
 :states '(insert emacs)
 :keymaps 'prog-mode-map
 "H-t" 'jester/expand-to-ternary
 "H-S-t" 'jester/expand-to-ternary-condensed)

;;----------------------------------------------------------------------------
;; Insert " = " for me, please.
;;----------------------------------------------------------------------------
(defun jester/make-javascript-assignment ()
  "Make a javascript assignment statement,
using things left of point as left value, things right as right value.

If nothing is at left, move point to the left value's position,
otherwise move to before semicolon.

If this line is already an assignment (has a \"=\"), cycle through styles in this order:
  an assignment without \"const\" or \"let\",
  a \"const\" assignment,
  a \"let\" assignment."
  (interactive)
  (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".*=.*$"))))
        (something-left-p (not (looking-back "^\s+"))))
    (save-excursion
      (if need-signs
          (progn (insert " = ") (move-end-of-line 1)
                 (unless (looking-back ";") (insert ";")))
        (beginning-of-line-text)
        (cond
         ((looking-at "const ") (kill-word 1) (insert "let"))
         ((looking-at "let ") (kill-word 1) (delete-char 1))
         (t (insert "const ")))))
    (when (and need-signs something-left-p) (move-end-of-line 1) (left-char))))

(general-define-key
 :states '(insert emacs)
 :keymaps '(web-mode-map js2-mode-map)
 "C-j" 'jester/make-javascript-assignment)

(defun jester/make-simple-assignment ()
  ""
  (interactive)
  (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".*=.*$"))))
        (something-left-p (not (looking-back "^\s+"))))
    (save-excursion
      (when need-signs
          (progn (insert " = "))))
    (when (and need-signs something-left-p) (move-end-of-line 1))))

(general-define-key
 :states '(insert emacs)
 :keymaps 'prog-mode-map
 "C-j" 'jester/make-simple-assignment)

;;----------------------------------------------------------------------------
;; Insert "this.".
;;----------------------------------------------------------------------------
(defun jester/insert-this. ()
  "Insert \"this.\" at point."
  (interactive)
  (insert "this."))

(general-define-key
 :states '(insert emacs)
 :keymaps '(web-mode-map js2-mode-map)
 "<C-i>" 'jester/insert-this.
 "<C-i>" 'jester/insert-this.)
;; TODO
;; foo             "1 xx"  ->      foo {
;; bar: 12                           bar: 12
;;                                 }

;;----------------------------------------------------------------------------
;; insert a curly block
;;----------------------------------------------------------------------------
;; https://stackoverflow.com/a/22114743/4788022
(defun jester/insert-curly-and-go-inside ()
  "Insert {}.
Threat is as function body when from endline before )"
  (interactive)
  (insert " {\n\n}") (indent-according-to-mode)
  (forward-line -1) (indent-according-to-mode))

(general-define-key
 :states '(insert emacs)
 :keymaps 'prog-mode-map
 "<C-return>" 'jester/insert-curly-and-go-inside)


(provide 'init-editing-utils)
