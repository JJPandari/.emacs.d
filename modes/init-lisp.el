(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(general-define-key [remap eval-expression] 'pp-eval-expression)

(when (maybe-require-package 'ipretty)
  (add-hook 'after-init-hook 'ipretty-mode))


(defun jester/maybe-set-bundled-elisp-readonly ()
  "If this elisp appears to be part of Emacs, then disallow editing."
  (when (and (buffer-file-name)
             (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
    (setq buffer-read-only t)
    (view-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'jester/maybe-set-bundled-elisp-readonly)

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)

;;----------------------------------------------------------------------------
;; Support byte-compilation in a sub-process, as required by highlight-cl
;;----------------------------------------------------------------------------
(defun jester/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
       " ")))))

;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun jester/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(add-hook! 'jester/lispy-modes-hook 'jester/enable-check-parens-on-save)

;;----------------------------------------------------------------------------
;; setup the lispy hooks
;;----------------------------------------------------------------------------
(defun jester/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'jester/lispy-modes-hook))

(defun jester/elisp-setup ()
  "Enable features useful elisp modes."
  (run-hooks 'jester/elispy-modes-hook))

(defconst jester/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst jester/lispy-modes
  (append jester/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name jester/lispy-modes))
  (add-hook hook 'jester/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name jester/elispy-modes))
  (add-hook hook 'jester/elisp-setup))


(use-package cl-lib-highlight
  :hook (lisp-mode . cl-lib-highlight-initialize))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)



;; Extras for theme editing

(defvar jester/theme-mode-hook nil
  "Hook triggered when editing a theme file.")

(defun jester/run-theme-mode-hooks-if-theme ()
  "Run `jester/theme-mode-hook' if this appears to a theme."
  (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
    (run-hooks 'jester/theme-mode-hook)))

(add-hook 'emacs-lisp-mode-hook 'jester/run-theme-mode-hooks-if-theme t)

(when (maybe-require-package 'rainbow-mode)
  (add-hook 'jester/theme-mode-hook 'rainbow-mode)
  (add-hook 'help-mode-hook 'rainbow-mode))

(after-load 'aggressive-indent
  ;; Can be prohibitively slow with very long forms
  (add-to-list 'jester/theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))


(defun jester/cl-libify-next ()
  "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
  (interactive)
  (let ((case-fold-search nil))
    (re-search-forward
     (concat
      "("
      (regexp-opt
       ;; Not an exhaustive list
       '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
         "case" "destructuring-bind" "second" "third" "defun*"
         "defmacro*" "return-from" "labels" "cadar" "fourth"
         "cadadr") t)
      "\\_>")))
  (let ((form (match-string 1)))
    (backward-sexp)
    (cond
     ((string-match "^\\(defun\\|defmacro\\)\\*$")
      (kill-sexp)
      (insert (concat "cl-" (match-string 1))))
     (t
      (insert "cl-")))
    (when (fboundp 'aggressive-indent-indent-defun)
      (aggressive-indent-indent-defun))))


;; show docstring after param list when point is on a function
(advice-add
 'elisp-get-fnsym-args-string :around
 (lambda (oldfun sym &rest args)
   "If SYM is a function, append its docstring."
   (concat
    (apply oldfun sym args)
    (let* ((doc (and (fboundp sym) (documentation sym 'raw)))
           (oneline (and doc (substring doc 0 (string-match "\n" doc)))))
      (and oneline
           (stringp oneline)
           (not (string= "" oneline))
           (concat "  |  " (propertize oneline 'face 'italic))))))
 '((name . "docstring")))


(jester/with-major-leader '(emacs-lisp-mode-map lisp-mode-map)
                          "e" 'eval-last-sexp
                          "f" 'eval-defun
                          "i" 'ielm)

(general-define-key
 :states '(emacs insert)
 :keymaps '(emacs-lisp-mode-map lisp-mode-map)
 "C-;" (lambda! (insert ";; ")))


(use-package lispyville
  :bind (("M-r" . lispy-raise) ("H-r" . lispy-raise-some))
  :init
  (add-hook! (emacs-lisp-mode lisp-mode) (flycheck-mode -1))
  (add-hook! 'emacs-lisp-mode-hook (setq mode-name "ELisp"))
  ;; load it everywhere
  ;; :hook (prog-mode . lispyville-mode)
  :hook ((emacs-lisp-mode lisp-mode) . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     ;; manually define prettify theme for lispy modes
     ;; prettify
     ;; "a" key in text-objects scheme conflicts with `jester/evil-a-attribute', setup manually
     ;; text-objects
     (additional-movement normal visual motion)
     (slurp/barf-cp normal visual)))

  ;; manually set text objects
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "l" 'lispyville-inner-list
   "f" 'lispyville-inner-function)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "l" 'lispyville-a-list
   "f" 'lispyville-a-function)

  ;; manually set prettify key
  (jester/with-leader
   ","
   (general-predicate-dispatch 'evil-indent
     (memq major-mode '(emacs-lisp-mode lisp-mode)) 'lispyville-prettify)))

(use-package macrostep
  :commands macrostep-expand)
;; TODO keys: override evil


(provide 'init-lisp)
