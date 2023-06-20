;; -*- lexical-binding: t -*-
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
      ;; jumps around a vc conflicted file like crazy if I enable this :(
      auto-revert-check-vc-info nil
      auto-revert-verbose nil
      initial-major-mode 'emacs-lisp-mode
      kill-ring-max 200)

(electric-indent-mode -1) ;; this is global
(use-package aggressive-indent
  :custom ((aggressive-indent-region-function 'evil-indent "also convert tab/space when indent")
           (aggressive-indent-sit-for-time 0.1))
  :hook (after-init . aggressive-indent-global-mode))

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

;;----------------------------------------------------------------------------
;; undo
;;----------------------------------------------------------------------------
(use-package undo-fu
  :init
  (general-define-key
   "C-/" 'undo-fu-only-undo
   "C-?" 'undo-fu-only-redo)
  :demand t
  :config
  (customize-set-variable 'evil-undo-system 'undo-fu))
(use-package undo-fu-session
  :demand t
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

(use-package vundo
  :init
  (jester/with-leader "v u" 'vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))


(use-package expand-region
  :commands er/expand-region
  :init
  (general-define-key
   "C-l" 'er/expand-region))


(use-package page-break-lines
  :demand t
  :config
  (global-page-break-lines-mode))


(use-package string-inflection)


(use-package subword
  :ensure nil
  :straight nil
  :init
  (general-define-key
   :states '(operator)
   "x" 'subword-forward
   "z" 'subword-backward)

  (evil-define-text-object jester/evil-inner-subword (count &optional beg end type)
    "Select a subword."
    :extend-selection nil
    (save-excursion (let ((start (progn (subword-backward) (point)))
                          (end (progn (subword-forward) (point))))
                      (list start end))))
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "x" 'jester/evil-inner-subword
   "z" 'jester/evil-inner-subword)

  :commands (subword-forward subword-backward jester/evil-inner-subword))


(use-package electric-operator
  :hook (;; (js2-mode . electric-operator-mode)
         ;; (css-mode . electric-operator-mode)
         (sass-mode . electric-operator-mode)
         ;; (rust-mode . electric-operator-mode)
         (java-mode . electric-operator-mode)
         (python-mode . electric-operator-mode)
         (sql-mode . electric-operator-mode)
         (c-mode . electric-operator-mode)
         (php-mode . electric-operator-mode)))


(use-package eldoc-box
  :demand t
  :after prog-mode
  :config
  (add-hook 'prog-mode-hook 'eldoc-box-hover-mode))

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
   "<tab>" 'symbol-overlay-put
   "H-n" 'symbol-overlay-jump-next
   "H-p" 'symbol-overlay-jump-prev)
  (jester/with-leader "o p" 'symbol-overlay-put)
  ;; don't bind any key
  (setq symbol-overlay-map (make-sparse-keymap)))

;;----------------------------------------------------------------------------
;; face related key bindings
;;----------------------------------------------------------------------------
(jester/with-leader
 "t c" 'text-scale-adjust)

;;----------------------------------------------------------------------------
;; smart ; key
;;----------------------------------------------------------------------------
;; TODO use tree-sitter
;; (defun jester/insert-\;or-: ()
;;   "Insert a \";\" or \":\" according to context."
;;   (interactive)jester-lispy-modes
;;   (cond ((memq major-mode jester-lispy-modes) (insert ":"))
;;         ((derived-mode-p 'prog-mode) (if (eolp)
;;                                          (insert ";")
;;                                        (insert ":")))
;;         (t (insert ";"))))

;; (defun jester/toggle-char-before-point-\;-or-: ()
;;   "Toggle the char before point between \";\" or \":\"."
;;   (interactive)
;;   (pcase (char-before)
;;     (?\; (delete-backward-char 1) (insert ":"))
;;     (?\: (delete-backward-char 1) (insert ";"))
;;     (_ (message "toggle \";\" or \":\": char before is neither \";\" nor \":\""))))

;; (general-define-key
;;  :states '(insert emacs)
;;  :keymaps 'prog-mode-map
;;  ";" 'jester/insert-\;or-:
;;  ":" 'jester/toggle-char-before-point-\;-or-:)

;;****************************************************************************
;; functions
;;****************************************************************************
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
 "C-:" 'insert-or-remove-trailing-comma)
;; TODO merge these two, use treesit to know which to do

;;----------------------------------------------------------------------------
;; Move lines up and down.
;;----------------------------------------------------------------------------
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
;; Insert " = " for me, please.
;;----------------------------------------------------------------------------
(defmacro jester/def-make-assignment-function (fun-name eol-str &rest forms-for-bolt)
  "Define a function with the name of `FUN-NAME'.
`FORMS-FOR-BOLT' is some forms evaluated when point is at beginning of line text,
who decides which type of assignment we currently have, and change it to the next one.
Use `EOL-STR' as the tail."
  `(defun ,fun-name ()
     "Make a mode specific assignment statement,
using things left of point as left value, things right as right value.

If nothing is at left, move point to the left value's position,
otherwise move to before semicolon.

If this line is already an assignment (has a \"=\"), cycle through some styles."
     (interactive)
     (save-match-data
       (let ((need-signs (save-excursion
                           (beginning-of-line-text) (not (looking-at ".* = .*$"))))
             (something-left-p (not (looking-back "^\s*"))))
         (save-excursion
           (if need-signs
               (progn (insert " = ") (move-end-of-line 1)
                      (unless (looking-back ,eol-str) (insert ,eol-str)))
             (beginning-of-line-text)
             ,@forms-for-bolt))
         (when (and need-signs something-left-p) (move-end-of-line 1) (left-char))))))

(defvar jester-javascript-assignment-declarer
  'let
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
  (save-match-data
    (let ((need-signs (save-excursion (beginning-of-line-text) (not (looking-at ".* = .*$"))))
          (something-left-p (not (looking-back "^\s*"))))
      (save-excursion
        (if need-signs
            (progn (insert " = ") (move-end-of-line 1)
                   (unless (looking-back ";") (insert ";")))
          (beginning-of-line-text)
          (pcase jester-javascript-assignment-declarer
            ('let (cond
                   ((looking-at "const ") (replace-match "let "))
                   ((looking-at "let ") (replace-match ""))
                   (t (insert "const "))))
            ('var (cond
                   ((looking-at "var ") (replace-match ""))
                   (t (insert "var "))))
            (_ (user-error "Plz set `jester-javascript-assignment-declarer' to `let' or `var'")))))
      (when (and need-signs something-left-p) (move-end-of-line 1) (left-char)))))

(general-define-key
 :states '(insert emacs)
 :keymaps '(web-mode-map js2-mode-map typescript-mode-map)
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
  "Insert {} on separate lines, and a new line in between."
  (interactive)
  (unless (looking-back "[ ({\\[]") (insert " "))
  (insert "{\n\n}") (indent-according-to-mode)
  (forward-line -1) (indent-according-to-mode))

(general-define-key
 :states '(insert emacs)
 :keymaps '(prog-mode-map conf-mode-map)
 "<C-return>" 'jester/insert-curly-and-go-inside)

;;----------------------------------------------------------------------------
;; insert a line below point
;;----------------------------------------------------------------------------
(defun jester/insert-newline-below ()
  "Insert a newline below point."
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (insert "\n")))

(general-define-key
 :states '(insert emacs)
 :keymaps '(prog-mode-map conf-mode-map)
 "<S-return>" 'jester/insert-newline-below)

;;----------------------------------------------------------------------------
;; move to bracket
;;----------------------------------------------------------------------------
(defvar jester-buffer-has-generic-or-jsx-p-list nil
  "Decides whether `jester/backward-bracket' and `jester/forward-bracket' will move to \"<\" and \">\",
by telling whether the language in current buffer has generic types (which are usually denoted by \"<T>\") or jsx.")
(push (lambda () (eq major-mode 'rust-mode)) jester-buffer-has-generic-or-jsx-p-list)
(push (lambda () (eq major-mode 'typescript-mode)) jester-buffer-has-generic-or-jsx-p-list)
(push (lambda () (eq major-mode 'rjsx-mode)) jester-buffer-has-generic-or-jsx-p-list)
(push (lambda () (and (eq major-mode 'web-mode) (string-equal web-mode-engine "none"))) jester-buffer-has-generic-or-jsx-p-list)
(push (lambda () (and (buffer-file-name)
                 (member (file-name-extension (buffer-file-name))
                         '("ts" "tsx"))))
      jester-buffer-has-generic-or-jsx-p-list)
(evil-define-motion jester/backward-bracket (count)
  "Move backward to a (, [ or {. Maybe <, if any of `jester-buffer-has-generic-or-jsx-p-list' yields t."
  ;; TODO enable lispyville everywhere
  :type exclusive
  (setq count (or count 1))
  (search-backward-regexp (if (cl-some (lambda (p) (funcall p)) jester-buffer-has-generic-or-jsx-p-list)
                              "[([{<]"
                            "[([{]")
                          nil t count))

(evil-define-motion jester/forward-bracket (count)
  "Move forward to a ), ] or }. Maybe >, if any of `jester-buffer-has-generic-or-jsx-p-list' yields t."
  :type exclusive
  (setq count (or count 1))
  (forward-char)
  (search-forward-regexp (if (cl-some (lambda (p) (funcall p)) jester-buffer-has-generic-or-jsx-p-list)
                             "[]})>]"
                           "[]})]")
                         nil t count)
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
  (next-line)
  (forward-paragraph count)
  (previous-line))

(evil-define-motion jester/backward-paragraph (count)
  "Move to paragraph start, line-wise."
  :type line
  (interactive "p")
  (previous-line)
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
(defun narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file."
  (if (region-active-p) (deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer
          (clone-indirect-buffer
           (generate-new-buffer-name
            (format "%s-indirect-:%s-:%s"
                    (buffer-name) (line-number-at-pos start t) (line-number-at-pos end t)))
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
(evil-define-text-object jester/evil-inner-subsentence (count &optional beg end type)
  "Select a subsentence(delimitered by comma), without the punctuations."
  :extend-selection nil
  (list (progn
          (re-search-backward "[,.] ?")
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (progn (re-search-forward "[,.] ?")
               (backward-char)
               (when (memq (char-before) (list ?. ?,)) (backward-char))
               (point))))

(evil-define-text-object jester/evil-a-subsentence (count &optional beg end type)
  "Select a subsentence(delimitered by comma)."
  :extend-selection nil
  (list (progn
          (re-search-backward "[,.] ?")
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (re-search-forward "[,.] ?")))

(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "S" 'jester/evil-inner-subsentence)
(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "S" 'jester/evil-a-subsentence)

;;----------------------------------------------------------------------------
;; select an argument
;;----------------------------------------------------------------------------
(evil-define-text-object jester/evil-inner-arg (count &optional beg end type)
  "Select an argument, without the punctuations."
  :extend-selection nil
  (list (progn
          (re-search-backward (rx (sequence (or ","
                                                "("
                                                "["
                                                "{")
                                            (optional " "))))
          (forward-char)
          (when (eq (char-after (point)) ? ) (forward-char))
          (point))
        (progn (re-search-forward (rx (or (sequence ","
                                                    (optional " "))
                                          (sequence (optional " ")
                                                    (or ")"
                                                        "]"
                                                        "}")))))
               (while (memq (char-before) (list ?, ?  ?\) ?\] ?})) (backward-char))
               (point))))

(evil-define-text-object jester/evil-a-arg (count &optional beg end type)
  "Select an argument."
  :extend-selection nil
  (let* (last-arg-p
         (head (progn
                 (re-search-backward (rx (sequence (or ","
                                                       "("
                                                       "["
                                                       "{")
                                                   (optional " "))))
                 (forward-char)
                 (when (eq (char-after (point)) ? ) (forward-char))
                 (point)))
         (tail (progn (re-search-forward (rx (or (sequence ","
                                                           (optional " "))
                                                 (sequence (optional " ")
                                                           (or ")"
                                                               "]"
                                                               "}")))))
                      (setq last-arg-p (memq (char-before) (list ?\) ?\] ?})))
                      (when (memq (char-before) (list ?\) ?\] ?}))
                        (while (memq (char-before) (list ?  ?\) ?\] ?})) (backward-char)))
                      (point))))
    (when last-arg-p
      (setq head (progn (goto-char head)
                        (while (memq (char-before) (list ?, ? )) (backward-char))
                        (point))))
    (list head tail)))

;;----------------------------------------------------------------------------
;; "just do what I mean"
;;----------------------------------------------------------------------------
(defvar jester-flip-symbol-alist
  '(("true" . "false")
    ("false" . "true")
    ("GET" . "POST")
    ("POST" . "GET")
    ("when" . "unless")
    ("unless" . "when"))
  "symbols to be quick flipped when editing")

(defun jester/just-do-what-i-mean ()
  "\"I don't want to type here, just do it for me.\" (check source for what this function does)"
  (interactive)
  (-let* (((beg . end) (bounds-of-thing-at-point 'symbol))
          (sym (buffer-substring-no-properties beg end)))
    (when (member sym (cl-loop for cell in jester-flip-symbol-alist
                               collect (car cell)))
      (delete-region beg end)
      (insert (alist-get sym jester-flip-symbol-alist "" nil 'string-equal)))))

(jester/with-leader
 "SPC" 'jester/just-do-what-i-mean)

;;----------------------------------------------------------------------------
;; show which function
;;----------------------------------------------------------------------------
(defun jester/show-which-function ()
  "Show which function in minibuffer."
  (interactive)
  (message (which-function)))

(jester/with-leader
 "f w" 'jester/show-which-function)

;;----------------------------------------------------------------------------
;; just one empty line
;;----------------------------------------------------------------------------
(defun jester/just-one-empty-line ()
  "Trim empty lines around point to be just one."
  (interactive)
  (while (and (looking-at "\s*\n")
              (not (equal (point) (point-min))))
    (beginning-of-line 0))
  (end-of-line)
  (while (looking-at "[\s\n]")
    (delete-forward-char 1))
  (insert "\n\n")
  (indent-according-to-mode)
  (beginning-of-line 0))

;;----------------------------------------------------------------------------
;; capitalize words
;;----------------------------------------------------------------------------
(evil-define-operator jester/evil-capitalize (beg end)
  "Capitalize words."
  (capitalize-region beg end))

(jester/with-leader
 "`" 'jester/evil-capitalize)

;;----------------------------------------------------------------------------
;; sync buffer to another file
;;----------------------------------------------------------------------------
(defun jester/sync-current-buffer-to-another-file ()
  "Sync current buffer to another file. (everytime after save)
To stop it, just revisit the file so the buffer local hook gets cleared."
  (interactive)
  ;; FIXME: no clean up / remove hook yet
  (let ((target-file (read-from-minibuffer "Sync current buffer to file: ")))
    (add-hook 'after-save-hook
              (lambda () (write-region (point-min) (point-max) target-file))
              0 t)
    (message "keeping syncing...")))


(provide 'init-editing-utils)
