;; (require-package 'elisp-slime-nav)
;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;   (add-hook hook 'turn-on-elisp-slime-nav-mode))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "ELisp")))

(setq-default initial-scratch-message
              (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))


;; 
;; ;; Make C-x C-e run 'eval-region if the region is active

;; (defun jester/eval-last-sexp-or-region (prefix)
;;   "Eval region from BEG to END if active, otherwise the last sexp."
;;   (interactive "P")
;;   (if (and (mark) (use-region-p))
;;       (eval-region (min (point) (mark)) (max (point) (mark)))
;;     (pp-eval-last-sexp prefix)))

;; (global-set-key [remap eval-expression] 'pp-eval-expression)

;; (after-load 'lisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-x C-e") 'jester/eval-last-sexp-or-region))

;; (when (maybe-require-package 'ipretty)
;;   (add-hook 'after-init-hook 'ipretty-mode))


;; (defadvice pp-display-expression (after jester/make-read-only (expression out-buffer-name) activate)
;;   "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
;;   (when (get-buffer out-buffer-name)
;;     (with-current-buffer out-buffer-name
;;       (view-mode 1))))

;; 

;; (defun jester/maybe-set-bundled-elisp-readonly ()
;;   "If this elisp appears to be part of Emacs, then disallow editing."
;;   (when (and (buffer-file-name)
;;              (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
;;     (setq buffer-read-only t)
;;     (view-mode 1)))

;; (add-hook 'emacs-lisp-mode-hook 'jester/maybe-set-bundled-elisp-readonly)

;; 
;; ;; Use C-c C-z to toggle between elisp files and an ielm session
;; ;; I might generalise this to ruby etc., or even just adopt the repl-toggle package.

;; (defvar jester/repl-original-buffer nil
;;   "Buffer from which we jumped to this REPL.")
;; (make-variable-buffer-local 'jester/repl-original-buffer)

;; (defvar jester/repl-switch-function 'switch-to-buffer-other-window)

;; (defun jester/switch-to-ielm ()
;;   (interactive)
;;   (let ((orig-buffer (current-buffer)))
;;     (if (get-buffer "*ielm*")
;;         (funcall jester/repl-switch-function "*ielm*")
;;       (ielm))
;;     (setq jester/repl-original-buffer orig-buffer)))

;; (defun jester/repl-switch-back ()
;;   "Switch back to the buffer from which we reached this REPL."
;;   (interactive)
;;   (if jester/repl-original-buffer
;;       (funcall jester/repl-switch-function jester/repl-original-buffer)
;;     (error "No original buffer")))

;; (after-load 'elisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'jester/switch-to-ielm))
;; (after-load 'ielm
;;   (define-key ielm-map (kbd "C-c C-z") 'jester/repl-switch-back))

;; ;; ----------------------------------------------------------------------------
;; ;; Hippie-expand
;; ;; ----------------------------------------------------------------------------

;; (defun set-up-hippie-expand-for-elisp ()
;;   "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
;;   (make-local-variable 'hippie-expand-try-functions-list)
;;   (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
;;   (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t)
;;   (add-to-list 'hippie-expand-try-functions-list 'my/try-complete-lisp-symbol-without-namespace t))


;; ;; ----------------------------------------------------------------------------
;; ;; Automatic byte compilation
;; ;; ----------------------------------------------------------------------------
;; (when (maybe-require-package 'auto-compile)
;;   (add-hook 'after-init-hook 'auto-compile-on-save-mode)
;;   (add-hook 'after-init-hook 'auto-compile-on-load-mode))

;; ;; ----------------------------------------------------------------------------
;; ;; Load .el if newer than corresponding .elc
;; ;; ----------------------------------------------------------------------------
;; (setq load-prefer-newer t)

;; 

;; (require-package 'immortal-scratch)
;; (add-hook 'after-init-hook 'immortal-scratch-mode)

;; 
;; ;;; Support byte-compilation in a sub-process, as
;; ;;; required by highlight-cl

;; (defun jester/byte-compile-file-batch (filename)
;;   "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
;;   (interactive "fFile to byte-compile in batch mode: ")
;;   (let ((emacs (car command-line-args)))
;;     (compile
;;      (concat
;;       emacs " "
;;       (mapconcat
;;        'shell-quote-argument
;;        (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
;;        " ")))))


;; ;; ----------------------------------------------------------------------------
;; ;; Enable desired features for all lisp modes
;; ;; ----------------------------------------------------------------------------
;; (defun jester/enable-check-parens-on-save ()
;;   "Run `check-parens' when the current buffer is saved."
;;   (add-hook 'after-save-hook #'check-parens nil t))

;; (defun jester/disable-indent-guide ()
;;   (when (bound-and-true-p indent-guide-mode)
;;     (indent-guide-mode -1)))

;; (defvar jester/lispy-modes-hook
;;   '(enable-paredit-mode
;;     turn-on-eldoc-mode
;;     jester/disable-indent-guide
;;     jester/enable-check-parens-on-save)
;;   "Hook run in all Lisp modes.")


;; (when (maybe-require-package 'aggressive-indent)
;;   (add-to-list 'jester/lispy-modes-hook 'aggressive-indent-mode))

;; (defun jester/lisp-setup ()
;;   "Enable features useful in any Lisp mode."
;;   (run-hooks 'jester/lispy-modes-hook))

;; (defun jester/emacs-lisp-setup ()
;;   "Enable features useful when working with elisp."
;;   (set-up-hippie-expand-for-elisp))

;; (defconst jester/elispy-modes
;;   '(emacs-lisp-mode ielm-mode)
;;   "Major modes relating to elisp.")

;; (defconst jester/lispy-modes
;;   (append jester/elispy-modes
;;           '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
;;   "All lispy major modes.")

;; (require 'derived)

;; (dolist (hook (mapcar #'derived-mode-hook-name jester/lispy-modes))
;;   (add-hook hook 'jester/lisp-setup))

;; (dolist (hook (mapcar #'derived-mode-hook-name jester/elispy-modes))
;;   (add-hook hook 'jester/emacs-lisp-setup))

;; (if (boundp 'eval-expression-minibuffer-setup-hook)
;;     (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
;;   (require-package 'eldoc-eval)
;;   (require 'eldoc-eval)
;;   (add-hook 'after-init-hook 'eldoc-in-minibuffer-mode))

;; (add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
;; (add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

;; (require-package 'cl-lib-highlight)
;; (after-load 'lisp-mode
;;   (cl-lib-highlight-initialize))

;; ;; ----------------------------------------------------------------------------
;; ;; Delete .elc files when reverting the .el from VC or magit
;; ;; ----------------------------------------------------------------------------

;; ;; When .el files are open, we can intercept when they are modified
;; ;; by VC or magit in order to remove .elc files that are likely to
;; ;; be out of sync.

;; ;; This is handy while actively working on elisp files, though
;; ;; obviously it doesn't ensure that unopened files will also have
;; ;; their .elc counterparts removed - VC hooks would be necessary for
;; ;; that.

;; (defvar jester/vc-reverting nil
;;   "Whether or not VC or Magit is currently reverting buffers.")

;; (defadvice revert-buffer (after jester/maybe-remove-elc activate)
;;   "If reverting from VC, delete any .elc file that will now be out of sync."
;;   (when jester/vc-reverting
;;     (when (and (eq 'emacs-lisp-mode major-mode)
;;                buffer-file-name
;;                (string= "el" (file-name-extension buffer-file-name)))
;;       (let ((elc (concat buffer-file-name "c")))
;;         (when (file-exists-p elc)
;;           (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
;;           (delete-file elc))))))

;; (defadvice magit-revert-buffers (around jester/reverting activate)
;;   (let ((jester/vc-reverting t))
;;     ad-do-it))
;; (defadvice vc-revert-buffer-internal (around jester/reverting activate)
;;   (let ((jester/vc-reverting t))
;;     ad-do-it))


;; 
;; (require-package 'macrostep)

;; (after-load 'lisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

;; 

;; ;; A quick way to jump to the definition of a function given its key binding
;; (global-set-key (kbd "C-h K") 'find-function-on-key)

;; 

;; ;; Extras for theme editing

;; (defvar jester/theme-mode-hook nil
;;   "Hook triggered when editing a theme file.")

;; (defun jester/run-theme-mode-hooks-if-theme ()
;;   "Run `jester/theme-mode-hook' if this appears to a theme."
;;   (when (string-match "\\(color-theme-\\|-theme\\.el\\)" (buffer-name))
;;     (run-hooks 'jester/theme-mode-hook)))

;; (add-hook 'emacs-lisp-mode-hook 'jester/run-theme-mode-hooks-if-theme t)

;; (when (maybe-require-package 'rainbow-mode)
;;   (add-hook 'jester/theme-mode-hook 'rainbow-mode)
;;   (add-hook 'help-mode-hook 'rainbow-mode))

;; (when (maybe-require-package 'aggressive-indent)
;;   ;; Can be prohibitively slow with very long forms
;;   (add-to-list 'jester/theme-mode-hook (lambda () (aggressive-indent-mode -1)) t))

;; 

;; (when (maybe-require-package 'highlight-quoted)
;;   (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; 
;; (when (maybe-require-package 'flycheck)
;;   (require-package 'flycheck-package)
;;   (after-load 'flycheck
;;     (flycheck-package-setup)))


;; 
;; ;; ERT
;; (after-load 'ert
;;   (define-key ert-results-mode-map (kbd "g") 'ert-results-rerun-all-tests))

;; 
;; (defun jester/cl-libify-next ()
;;   "Find next symbol from 'cl and replace it with the 'cl-lib equivalent."
;;   (interactive)
;;   (let ((case-fold-search nil))
;;     (re-search-forward
;;      (concat
;;       "("
;;       (regexp-opt
;;        ;; Not an exhaustive list
;;        '("loop" "incf" "plusp" "first" "decf" "minusp" "assert"
;;          "case" "destructuring-bind" "second" "third" "defun*"
;;          "defmacro*" "return-from" "labels" "cadar" "fourth"
;;          "cadadr") t)
;;       "\\_>")))
;;   (let ((form (match-string 1)))
;;     (backward-sexp)
;;     (cond
;;      ((string-match "^\\(defun\\|defmacro\\)\\*$")
;;       (kill-sexp)
;;       (insert (concat "cl-" (match-string 1))))
;;      (t
;;       (insert "cl-")))
;;     (when (fboundp 'aggressive-indent-indent-defun)
;;       (aggressive-indent-indent-defun))))


;; (maybe-require-package 'cask-mode)


(use-package lispyville
  :hook ((emacs-lisp-mode lisp-mode) . lispyville-mode)
  :init
  (add-hook! (emacs-lisp-mode lisp-mode) (flycheck-mode -1))
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     prettify
     ;; "a" key in text-objects scheme conflicts with `jester/evil-a-attribute', setup manually
     ;; text-objects
     (additional-movement normal visual motion)
     (slurp/barf-cp normal visual)))

  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "l" 'lispyville-inner-list
   "f" 'lispyville-inner-function)
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "l" 'lispyville-a-list
   "f" 'lispyville-a-function)

  (general-define-key
   :states '(normal operator motion)
   :keymaps '(emacs-lisp-mode-map lisp-mode-map)
   "H" 'lispyville-backward-up-list
   "L" 'lispyville-up-list)

  (general-define-key
   :states '(emacs insert)
   :keymaps '(emacs-lisp-mode-map lisp-mode-map)
   "C-;" (lambda! (insert ";; ")))

  (jester/with-major-leader '(emacs-lisp-mode-map lisp-mode-map)
                            "e" 'eval-last-sexp
                            "d" 'eval-defun
                            "i" 'ielm))

(use-package macrostep
  :commands macrostep-expand)
;; TODO override evil


(provide 'init-lisp)
