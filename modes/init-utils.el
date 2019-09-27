(use-package s
  :demand t)

(use-package dash
  :demand t)

(use-package move-text
  :demand t)

;;----------------------------------------------------------------------------
;; `after-load'
;;----------------------------------------------------------------------------
(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;----------------------------------------------------------------------------
;; Interactive lambda shorthand.
;;----------------------------------------------------------------------------
(defmacro lambda! (&rest body)
  "A shortcut for inline interactive lambdas."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

;;----------------------------------------------------------------------------
;; Run in after-init-hook.
;;----------------------------------------------------------------------------
(defmacro after-init (&rest body)
  "A shortcut for adding to after-init-hook."
  (declare (doc-string 1))
  `(add-hook 'after-init-hook ,@body))

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun jester/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; get region or symbol
;;----------------------------------------------------------------------------
(defun jester/region-or-symbol ()
  "Get active region or symbol at point."
  (regexp-quote (if (region-active-p)
                    (let ((beg (region-beginning))
                          (end (region-end)))
                      (deactivate-mark)
                      (buffer-substring-no-properties
                       beg end))
                  (thing-at-point 'symbol t))))

;;----------------------------------------------------------------------------
;; Merge imenus.
;;----------------------------------------------------------------------------
;; https://stackoverflow.com/a/21656063/4788022
(defun jester/merge-imenu (INDEX-FUN)
  "Merge major-mode-provided imenu, which is generated by `INDEX-FUN',
with traditional regex based imenu."
  (interactive)
  (let ((mode-imenu (funcall INDEX-FUN))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append custom-imenu mode-imenu)))

;;----------------------------------------------------------------------------
;; Get the list of matches when matching a regexp against a string.
;;----------------------------------------------------------------------------
;; https://emacs.stackexchange.com/a/7150/12854
(defun jester/regexp-matches-in-string (regexp string nth-group)
  "Get a list of all `REGEXP' matches in `STRING', taking `NTH-GROUP' every time we match."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string nth-group string) matches)
        (setq pos (match-end 0)))
      (nreverse matches))))

;;----------------------------------------------------------------------------
;; Select the emacs frame (used in shell scripts).
;;----------------------------------------------------------------------------
(defun open-emacs-window ()
  "Switch to emacs frame."
  (select-frame-set-input-focus (selected-frame)))

;;----------------------------------------------------------------------------
;; remove all advices.
;;----------------------------------------------------------------------------
(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

;;----------------------------------------------------------------------------
;; convert mode list to keymap list
;;----------------------------------------------------------------------------
(defun jester/mode-list-to-mode-map-list (modes)
  "Return a list of keymaps corresponding to the list `MODES'."
  (mapcar
   (lambda (mode) (intern (format "%s-map" mode)))
   modes))

;;----------------------------------------------------------------------------
;; eval and always display in same window (not used)
;;----------------------------------------------------------------------------
(defmacro jester/eval-with-display-in-same-window (&rest forms)
  "Eval `FORMS', with the behavior of `display-buffer' fixed to display in same window."
  (declare (indent defun))
  `(cl-flet ((display-buffer (buffer action) (display-buffer buffer '(display-buffer-same-window . nil))))
     ,@forms))

;;----------------------------------------------------------------------------
;; hash table to alist
;;----------------------------------------------------------------------------
(defun jester/hashtable-to-alist (table)
  "Convert `TABLE' to an alist."
  (let ((alist))
    (maphash
     (lambda (k v) (push (cons k v) alist))
     table)
    alist))

;;----------------------------------------------------------------------------
;; am I in expression but not comment/string?
;;----------------------------------------------------------------------------
(defun jester/in-expression-area-p ()
  "Am I in expression but not comment/string?"
  (pcase major-mode
    ('emacs-lisp-mode (not (lispy--in-string-or-comment-p)))
    ('js2-mode (let ((node (js2-node-at-point)))
                 (and (not (js2-comment-node-p node))
                      (not (js2-string-node-p node)))))))

;;----------------------------------------------------------------------------
;; add hook shorthand
;;----------------------------------------------------------------------------
(defmacro add-hook! (&rest args)
  "A convenience macro for `add-hook'. Takes, in order:

  1. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  2. The hooks: either an unquoted major mode, an unquoted list of major-modes,
     a quoted hook variable or a quoted list of hook variables. If unquoted, the
     hooks will be resolved by appending -hook to each symbol.
  3. A function, list of functions, or body forms to be wrapped in a lambda.

Examples:
    (add-hook! 'some-mode-hook 'enable-something)
    (add-hook! some-mode '(enable-something and-another))
    (add-hook! '(one-mode-hook second-mode-hook) 'enable-something)
    (add-hook! (one-mode second-mode) 'enable-something)
    (add-hook! :append (one-mode second-mode) 'enable-something)
    (add-hook! :local (one-mode second-mode) 'enable-something)
    (add-hook! (one-mode second-mode) (setq v 5) (setq a 2))
    (add-hook! :append :local (one-mode second-mode) (setq v 5) (setq a 2))

Body forms can access the hook's arguments through the let-bound variable
`args'."
  (declare (indent defun) (debug t))
  (let ((hook-fn 'add-hook)
        append-p local-p)
    (while (keywordp (car args))
      (pcase (pop args)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq hook-fn 'remove-hook))))
    (let ((hooks (doom--resolve-hook-forms (pop args)))
          (funcs
           (let ((val (car args)))
             (if (memq (car-safe val) '(quote function))
                 (if (cdr-safe (cadr val))
                     (cadr val)
                   (list (cadr val)))
               (list args))))
          forms)
      (dolist (fn funcs)
        (setq fn (if (symbolp fn)
                     `(function ,fn)
                   `(lambda (&rest _) ,@args)))
        (dolist (hook hooks)
          (push (if (eq hook-fn 'remove-hook)
                    `(remove-hook ',hook ,fn ,local-p)
                  `(add-hook ',hook ,fn ,append-p ,local-p))
                forms)))
      `(progn ,@(nreverse forms)))))

(defun doom--resolve-hook-forms (hooks)
  (cl-loop with quoted-p = (eq (car-safe hooks) 'quote)
           for hook in (doom-enlist (doom-unquote hooks))
           if (eq (car-safe hook) 'quote)
            collect (cadr hook)
           else if quoted-p
            collect hook
           else collect (intern (format "%s-hook" (symbol-name hook)))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (if (listp exp) exp (list exp)))


(provide 'init-utils)
