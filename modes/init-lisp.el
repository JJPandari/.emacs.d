(setq initial-scratch-message
      (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

;; (general-define-key [remap eval-expression] 'pp-eval-expression)
;; (when (maybe-require-package 'ipretty)
;;   (add-hook 'after-init-hook 'ipretty-mode))


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

(add-hook 'jester-lispy-modes-hook 'jester/enable-check-parens-on-save)
(after-load 'aggressive-indent
  (add-hook! 'jester-lispy-modes-hook
    (setq-local aggressive-indent-region-function 'lisp-indent-region)))

;;----------------------------------------------------------------------------
;; setup the lispy hooks
;;----------------------------------------------------------------------------
(defun jester/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'jester-lispy-modes-hook))

(defun jester/elisp-setup ()
  "Enable features useful elisp modes."
  (run-hooks 'jester-elispy-modes-hook))

(defconst jester-elispy-modes
  '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode)
  "Major modes relating to elisp.")

(defconst jester-lispy-modes
  (append jester-elispy-modes
          '(lisp-mode scheme-mode inferior-scheme-mode racket-mode))
  "Major modes relating to lisp.")

(defconst jester-elispy-maps
  (jester/mode-list-to-mode-map-list jester-elispy-modes)
  "Keymaps relating to elisp.")

(defconst jester-lispy-maps
  (jester/mode-list-to-mode-map-list jester-lispy-modes)
  "Keymaps relating to lisp.")

(require 'derived)

;; FIXME lisp setups are actually run twice here
(dolist (hook (mapcar #'derived-mode-hook-name jester-lispy-modes))
  (add-hook hook 'jester/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name jester-elispy-modes))
  (add-hook hook 'jester/elisp-setup))


(use-package cl-lib-highlight
  :hook (lisp-mode . cl-lib-highlight-initialize))



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


;; https://emacs-china.org/t/minibuffer-point-elisp/10048/6?u=jjpandari
;; show docstring after param list when point is on a function
(define-advice elisp-get-fnsym-args-string (:around (orig-fun sym &rest r) docstring)
  "If SYM is a function, append its docstring."
  (concat
   (apply orig-fun sym r)
   (when-let* ((doc (and (fboundp sym) (documentation sym 'raw)))
               (stripped-advice-doc (replace-regexp-in-string
                                     ;; regex from ivy-rich
                                     ":\\(\\(before\\|after\\)\\(-\\(while\\|until\\)\\)?\\|around\\|override\\|\\(filter-\\(args\\|return\\)\\)\\) advice:[ ]*‘.+?’[\r\n]+"
                                     ""
                                     doc))
               (oneline (substring stripped-advice-doc 0 (string-match "\n" stripped-advice-doc))))
     (when (not (string= "" oneline))
       (concat "  |  " (propertize oneline 'face 'italic))))))


(jester/with-major-leader jester-elispy-maps
  "e" 'eval-last-sexp
  "f" 'eval-defun
  "i" 'ielm)

(defun jester/insert-lisp-comment-start ()
  "Insert \";; \" or \"; \", based on whether at beginning of line."
  (interactive)
  (if (looking-back "^\s*")
      (insert ";; ")
    (insert "; ")))

(general-define-key
 :states '(emacs insert)
 :keymaps jester-lispy-maps
 "C-;" 'jester/insert-lisp-comment-start)


(push (expand-file-name "elispfl" jester-submodules-dir) load-path)
(use-package elispfl
  :ensure nil
  :demand t
  :config (elispfl-mode 1))


(use-package lispyville
  :bind (("M-r" . lispy-raise) ("H-r" . lispy-raise-some))
  :init
  (add-hook! (emacs-lisp-mode lisp-mode) (flycheck-mode -1))
  (add-hook! 'emacs-lisp-mode-hook (setq mode-name "ELisp"))
  (add-hook! 'jester-lispy-modes-hook (lispyville-mode 1))
  ;; load it everywhere
  ;; :hook (prog-mode . lispyville-mode)
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
   "f" 'lispyville-inner-function
   "c" 'lispyville-inner-comment)
  ;; `lispyville-inner-atom' is bound in init-web.el
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "l" 'lispyville-a-list
   "f" 'lispyville-a-function
   "c" 'lispyville-a-comment)

  ;; manually set prettify and comment key
  (jester/with-leader
   "," (general-predicate-dispatch 'evil-indent
         (memq major-mode jester-lispy-modes) 'lispyville-prettify)
   ";" (general-predicate-dispatch 'evilnc-comment-operator
         (memq major-mode jester-lispy-modes) 'lispyville-comment-or-uncomment))

  ;; some other lispy{,ville} keys
  (general-define-key
   :states '(emacs insert normal visual)
   :keymaps jester-lispy-maps
   "M-u" 'lispyville-wrap-round)
  ;; ugly fix
  (general-define-key
   :states '(emacs insert normal visual)
   :keymaps 'inferior-emacs-lisp-mode-map
   "M-r" 'lispy-raise))


(use-package macrostep
  :commands macrostep-expand
  :config
  (defhydra jester/hydra-macrostep (nil nil :foreign-keys run)
    "macrostep"
    ("e" macrostep-expand "expand")
    ("c" macrostep-collapse "collapse")
    ("q" macrostep-collapse-all "quit" :exit t))
  (add-hook! 'macrostep-mode-hook (jester/hydra-macrostep/body)))


(provide 'init-lisp)
