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

(electric-indent-mode -1) ;; this is global
(use-package aggressive-indent
  :custom (aggressive-indent-region-function 'evil-indent "also convert tab/space when indent")
  :hook (after-init . aggressive-indent-global-mode))

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


(use-package expand-region
  :commands er/expand-region
  :init
  (general-define-key
   "<C-backspace>" 'er/expand-region))


(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))


(use-package string-inflection)


(use-package subword
  :ensure nil
  :init
  (general-define-key
   :states '(operator)
   "x" 'subword-forward
   "z" 'subword-backward)
  :commands (subword-forward subword-backward))


(use-package electric-operator
  :hook ((js2-mode . electric-operator-mode)
         (css-mode . electric-operator-mode)
         (sass-mode . electric-operator-mode)
         (rust-mode . electric-operator-mode)
         (java-mode . electric-operator-mode)
         (python-mode . electric-operator-mode)
         (sql-mode . electric-operator-mode)
         (c-mode . electric-operator-mode)
         (php-mode . electric-operator-mode)))


;;----------------------------------------------------------------------------
;; kill back to indentation
;;----------------------------------------------------------------------------
(defun jester/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(general-define-key
 :states '(insert emacs)
 "C-S-k" 'jester/kill-back-to-indentation)

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
 :states '(normal visual)
 "z k" 'jester/move-line-up
 "z j" 'jester/move-line-down)

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
;; Make a "foo: bar," key-value pair
;;----------------------------------------------------------------------------
(defun jester/make-key-value-pair ()
  "Make a key-value pair, by inserting \": ,\" at point.
Effectively using symbol before point as the key."
  (interactive)
  (insert ": ,") (backward-char))

(general-define-key
 :states '(insert emacs)
 :keymaps 'prog-mode-map
 "<C-i>" 'jester/make-key-value-pair)

;;----------------------------------------------------------------------------
;; Make a "foo: bar;" key-value pair
;;----------------------------------------------------------------------------
(defun jester/make-css-pair ()
  "Make a key-value pair for css etc., by inserting \": ;\" at point."
  (interactive)
  (insert ": ;") (backward-char))

(general-define-key
 :states '(insert emacs)
 :keymaps 'prog-mode-map
 "M-;" 'jester/make-css-pair)

;;----------------------------------------------------------------------------
;; Insert " = " for me, please.
;;----------------------------------------------------------------------------
(defvar jester-javascript-assignment-declarer
  'var
  "What word to use when making a javascript assignment.
When set to `let', use \"let\" and \"const\".
When set to `var', use \"var\".")

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
  (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".* = .*$"))))
        (something-left-p (not (looking-back "^\s*"))))
    (save-excursion
      (if need-signs
          (progn (insert " = ") (move-end-of-line 1)
                 (unless (looking-back ";") (insert ";")))
        (beginning-of-line-text)
        (cond
         ((eq jester-javascript-assignment-declarer 'let)
          (cond
           ((looking-at "const ") (kill-word 1) (insert "let"))
           ((looking-at "let ") (kill-word 1) (delete-char 1))
           (t (insert "const "))))
         ((eq jester-javascript-assignment-declarer 'var)
          (cond
           ((looking-at "var ") (kill-word 1) (delete-char 1))
           (t (insert "var "))))
         (t (user-error "Plz set `jester-javascript-assignment-declarer' to `let' or `var'")))
        ))
    (when (and need-signs something-left-p) (move-end-of-line 1) (left-char))))

(general-define-key
 :states '(insert emacs)
 :keymaps '(web-mode-map js2-mode-map)
 "C-j" 'jester/make-javascript-assignment)

(defun jester/make-simple-assignment ()
  "Make a assignment statement,
using things left of point as left value, things right as right value.

If nothing is at left, move point to the left value's position,
otherwise move to before semicolon."
  (interactive)
  (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".* = .*$"))))
        (something-left-p (not (looking-back "^\s*"))))
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
 :keymaps '(web-mode-map js2-mode-map coffee-mode-map)
 "C-t" 'jester/insert-this.)

;;----------------------------------------------------------------------------
;; insert a curly block
;;----------------------------------------------------------------------------
;; https://stackoverflow.com/a/22114743/4788022
(defun jester/insert-curly-and-go-inside ()
  "Insert {}.
Threat is as function body when from endline before )"
  (interactive)
  (unless (looking-back "[ ({\\[]") (insert " "))
  (insert "{\n\n}") (indent-according-to-mode)
  (forward-line -1) (indent-according-to-mode))

(general-define-key
 :states '(insert emacs)
 :keymaps '(prog-mode-map conf-mode-map)
 "<C-return>" 'jester/insert-curly-and-go-inside)

;;----------------------------------------------------------------------------
;; move to bracket
;;----------------------------------------------------------------------------
(evil-define-motion jester/backward-bracket (count)
  "Move backward to a (, [ or {."
  ;; TODO enable lispyville everywhere
  :type exclusive
  (setq count (or count 1))
  (search-backward-regexp "[([{]" nil t count))

(evil-define-motion jester/forward-bracket (count)
  "Move forward to a ), ] or }."
  :type exclusive
  (setq count (or count 1))
  (forward-char)
  (search-forward-regexp "[]})]" nil t count)
  (backward-char))

(general-define-key
 :states '(normal visual motion operator)
 "(" 'jester/backward-bracket
 ")" 'jester/forward-bracket)

(jester/with-leader
 "(" 'backward-up-list
 ")" 'up-list)

;;----------------------------------------------------------------------------
;; move to paragraph begin/end
;;----------------------------------------------------------------------------
(evil-define-motion jester/forward-paragraph (count)
  "Move to paragraph end, line-wise."
  :type line
  (interactive "p")
  (forward-paragraph count)
  (previous-line))

(evil-define-motion jester/backward-paragraph (count)
  "Move to paragraph start, line-wise."
  :type line
  (interactive "p")
  (backward-paragraph count)
  (next-line))

(general-define-key
 :states '(normal visual motion operator)
 "[" 'jester/backward-paragraph
 "]" 'jester/forward-paragraph)

;;----------------------------------------------------------------------------
;; narrow
;;----------------------------------------------------------------------------
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-misc.el
(defun line-number-at-position (pos)
  "Returns the line number for position `POS'."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char pos)
      (+ 1 (count-lines (point-min) (line-beginning-position 1))))))

(defun narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file."
  (if (region-active-p) (deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer
          (clone-indirect-buffer
           (generate-new-buffer-name
            (format "%s-indirect-:%s-:%s"
                    (buffer-name) (line-number-at-position start) (line-number-at-position end)))
           'display)
        (narrow-to-region start end)
        (goto-char (point-min)))
    (narrow-to-region start end)))

;; https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
(defun narrow-or-widen-dwim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
 Otherwise, it narrows to region, or Org subtree.
If use-indirect-buffer is not nil, use `indirect-buffer' to hold the widen content."
  (interactive "P")
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p)
         (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                                 (region-end)
                                                 use-indirect-buffer))
        ((equal major-mode 'org-mode)
         (org-narrow-to-subtree))
        ((derived-mode-p 'diff-mode)
         (let* (b e)
           (save-excursion
             ;; If the (point) is already beginning or end of file diff,
             ;; the `diff-beginning-of-file' and `diff-end-of-file' return nil
             (setq b (progn (diff-beginning-of-file) (point)))
             (setq e (progn (diff-end-of-file) (point))))
           (when (and b e (< b e))
             (narrow-to-region-indirect-buffer-maybe b e use-indirect-buffer))))
        ((derived-mode-p 'prog-mode)
         (mark-defun)
         (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                                 (region-end)
                                                 use-indirect-buffer))
        (t (error "Please select a region to narrow to"))))

(jester/with-leader "n n" 'narrow-or-widen-dwim)

;;----------------------------------------------------------------------------
;; insert lorem
;;----------------------------------------------------------------------------
(defun insert-lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque sem mauris, aliquam vel interdum in, faucibus non libero. Asunt in anim uis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in anim id est laborum. Allamco laboris nisi ut aliquip ex ea commodo consequat."))

;;----------------------------------------------------------------------------
;; do something to a comma-delimitered sentence
;;----------------------------------------------------------------------------
(evil-define-text-object jester/evil-a-subsentence (count &optional beg end type)
  "Select a subsentence(delimitered by comma)."
  :extend-selection nil
  (list (progn
          (re-search-backward "[,.] ?")
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (re-search-forward "[,.] ?")))

(evil-define-text-object jester/evil-inner-subsentence (count &optional beg end type)
  "Select a subsentence(delimitered by comma), without the punctuations."
  :extend-selection nil
  (list (progn
          (re-search-backward "[,.] ?")
          ;; (re-search-backward "[,.] ?\\|( ?")
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (progn (re-search-forward "[,.] ?")
               (backward-char)
               (when (memq (char-before) (list ?. ?,)) (backward-char))
               (point))))

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "S" 'jester/evil-a-subsentence)
(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "S" 'jester/evil-inner-subsentence)

;;----------------------------------------------------------------------------
;; select an argument
;;----------------------------------------------------------------------------
(evil-define-text-object jester/evil-a-arg (count &optional beg end type)
  "Select an argument."
  :extend-selection nil
  (let* (last-arg-p
         (head (progn
                 (re-search-backward "[,(] ?")
                 (forward-char)
                 (when (eq (char-after (point)) ? ) (forward-char))
                 (point)))
         (tail (progn (re-search-forward "[,] ?\\| ?)")
                      (setq last-arg-p (eq (char-before) ?\)))
                      (when (eq (char-before) ?\))
                        (while (memq (char-before) (list ?  ?\))) (backward-char)))
                      (point))))
    (when last-arg-p
      (setq head (progn (goto-char head)
                        (while (memq (char-before) (list ?, ? )) (backward-char))
                        (point))))
    (list head tail)))

(evil-define-text-object jester/evil-inner-arg (count &optional beg end type)
  "Select an argument, without the punctuations."
  :extend-selection nil
  (list (progn
          (re-search-backward "[,(] ?")
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (progn (re-search-forward "[,] ?\\| ?)")
               (while (memq (char-before) (list ?, ?  ?\))) (backward-char))
               (point))))


(provide 'init-editing-utils)
