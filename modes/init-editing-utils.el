(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(maybe-require-package 'list-unicode-display)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(after-load 'autorevert
  (diminish 'auto-revert-mode))

(add-hook 'after-init-hook 'transient-mark-mode)



;; Huge files

(require-package 'vlf)

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


;;; A simple visible bell which works in all terminal types
(require-package 'mode-line-bell)
(add-hook 'after-init-hook 'mode-line-bell-mode)



(when (maybe-require-package 'beacon)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (add-hook 'after-init-hook 'beacon-mode))



;;; Newline behaviour

(global-set-key (kbd "RET") 'newline-and-indent)
(defun jester/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'jester/newline-at-end-of-line)



(after-load 'subword
  (diminish 'subword-mode))



(unless (fboundp 'display-line-numbers-mode)
  (require-package 'nlinum))


(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'after-init-hook 'global-prettify-symbols-mode))


(require-package 'undo-tree)
(add-hook 'after-init-hook 'global-undo-tree-mode)
(after-load 'undo-tree
  (diminish 'undo-tree-mode))


(when (maybe-require-package 'symbol-overlay)
  (dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook conf-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (after-load 'symbol-overlay
    (diminish 'symbol-overlay-mode)
    (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
    (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
    (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev)))

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)



(require-package 'browse-kill-ring)
(setq browse-kill-ring-separator "\f")
(global-set-key (kbd "M-Y") 'browse-kill-ring)
(after-load 'browse-kill-ring
  (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
  (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))
(after-load 'page-break-lines
  (push 'browse-kill-ring-mode page-break-lines-modes))


;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(when (maybe-require-package 'avy)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(require-package 'multiple-cursors)
;; multiple-cursors
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-+") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)


;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)


;;----------------------------------------------------------------------------
;; Page break lines
;;----------------------------------------------------------------------------
(when (maybe-require-package 'page-break-lines)
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (after-load 'page-break-lines
    (diminish 'page-break-lines-mode)))

;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down. When paredit is enabled,
;; it will use those keybindings. For this reason, you might prefer to
;; use M-S-up and M-S-down, which will work even in lisp modes.
;;----------------------------------------------------------------------------
(require-package 'move-dup)
(global-set-key [M-up] 'md/move-lines-up)
(global-set-key [M-down] 'md/move-lines-down)
(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c d") 'md/duplicate-down)
(global-set-key (kbd "C-c u") 'md/duplicate-up)

;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(require-package 'whole-line-or-region)
(add-hook 'after-init-hook 'whole-line-or-region-mode)
(after-load 'whole-line-or-region
  (diminish 'whole-line-or-region-local-mode))

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)




(defun jester/open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
         (do-left-margin (and (bolp) (> (current-left-margin) 0)))
         (loc (point-marker))
         ;; Don't expand an abbrev before point.
         (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
             (if do-left-margin (indent-to (current-left-margin)))
             (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key (kbd "C-o") 'jester/open-line-with-reindent)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))




(require-package 'highlight-escape-sequences)
(add-hook 'after-init-hook 'hes-mode)


(require-package 'guide-key)
(setq guide-key/guide-key-sequence t)
(add-hook 'after-init-hook 'guide-key-mode)
(after-load 'guide-key
  (diminish 'guide-key-mode))






;; https://emacs-china.org/t/topic/4494/25?u=jester
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

(define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
(define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))

(defun jester/move-line-up (count)
  "Move `prefix-arg' lines up."
  (interactive "p")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (beg-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end)))
        (evil-move beg end (- beg-line count 1)))
    (move-text-up (line-beginning-position) (line-beginning-position 2) count)
    (deactivate-mark)
    (indent-for-tab-command)))

(defun jester/move-line-down (count)
  "Move `prefix-arg' lines down."
  (interactive "p")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (beg-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end)))
        (evil-move beg end (+ beg-line count 1)))
    (move-text-down (line-beginning-position) (line-beginning-position 2) count)
    (deactivate-mark)
    (indent-for-tab-command)))

;; like web-mode-on-post-command
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
      (indent-according-to-mode)
      )
    ))

;; https://stackoverflow.com/a/21656063/4788022
(defun jester/merge-imenu (index-fun)
  (interactive)
  (let ((mode-imenu (funcall index-fun))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append custom-imenu mode-imenu)))

(defun jester/expand-to-ternary ()
  "Add a ternary expression, using the symbol before point as the first field."
  (interactive)
  (yas-expand-snippet " ? $1 : $0"))

(defun jester/expand-to-ternary-condensed ()
  "Add a (condensed) ternary expression, using the symbol before point as the first field."
  (interactive)
  (yas-expand-snippet "?$1:$0"))

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

(evil-define-key 'insert web-mode-map (kbd "C-j") 'jester/make-javascript-assignment)
(evil-define-key 'insert js2-mode-map (kbd "C-j") 'jester/make-javascript-assignment)

(defun jester/make-simple-assignment ()
  ""
  (interactive)
  (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".*=.*$"))))
        (something-left-p (not (looking-back "^\s+"))))
    (save-excursion
      (when need-signs
          (progn (insert " = "))))
    (when (and need-signs something-left-p) (move-end-of-line 1))))

(evil-define-key 'insert prog-mode-map (kbd "C-j") 'jester/make-simple-assignment)

(defun fontux/paredit-kill-backward (&optional argument)
  "Backward version of `paredit-kill'.

With a `\\[universal-argument]' prefix argument, kill the text before point on
the current line.
With a positive integer prefix argument N, kill lines backward
many times.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, kill all expressions
before the point in the current block, group, string or comment."
  (interactive "P")
  (let ((hungry-p (equal argument '(16))))
    (cond ((and (bolp) (not argument))
           (delete-char -1))
          ((and (integerp argument) (> argument 1))
           (kill-line (- (1- argument))))
          ((and argument (not hungry-p))
           (kill-line 0))
          (t (let* ((pos (point))
                    (bol (point-at-bol))
                    (cur-ppss (syntax-ppss))
                    (cur-up-pos (cadr cur-ppss)))
               (when (nth 5 cur-ppss) (cl-incf pos))
               (cond ((or (nth 3 cur-ppss) (and (nth 4 cur-ppss) (nth 7 cur-ppss)))
                      (let ((str/cmt-pos (1+ (nth 8 cur-ppss))))
                        (when (nth 7 cur-ppss) (cl-incf str/cmt-pos))
                        (kill-region (if hungry-p str/cmt-pos (max bol str/cmt-pos)) pos)))
                     ((and cur-up-pos (or hungry-p (<= bol cur-up-pos)))
                      (kill-region (1+ cur-up-pos) pos))
                     (t (let* ((bol-ppss (save-excursion (syntax-ppss bol)))
                               (bol-up-pos (cadr bol-ppss)))
                          (cond ((or (and cur-up-pos (> bol-up-pos cur-up-pos))
                                     (and (not cur-up-pos) bol-up-pos))
                                 (kill-region (nth (car cur-ppss) (nth 9 bol-ppss)) pos))
                                ((or (nth 3 bol-ppss) (nth 4 bol-ppss))
                                 (kill-region (nth 8 bol-ppss) pos))
                                (t (kill-region bol pos)))))))))))

;; TODO kbd "DL" delete whole lines when nothing before and only non-word after the kill end

(defun jester/insert-this. ()
  "Insert \"this.\" at point."
  (interactive)
  (insert "this."))
(evil-define-key 'insert web-mode-map (kbd "H-i") 'jester/insert-this.)
(evil-define-key 'insert js2-mode-map (kbd "H-i") 'jester/insert-this.)


(provide 'init-editing-utils)
