(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive t
        dumb-jump-selector 'ivy)
  (general-define-key
   :states '(normal motion)
   "g d" 'dumb-jump-go
   ;; "g p" 'dumb-jump-back
   "g p" 'xref-pop-marker-stack)
  :commands (dumb-jump-go))

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(defun jester/xref-find-references-at-point ()
  "Find references for symbol at point."
  (interactive)
  (xref-find-references (xref-backend-identifier-at-point (xref-find-backend))))

(general-define-key
 :states '(normal motion)
 :keymaps '(emacs-lisp-mode-map lisp-mode-map)
 "g d" 'xref-find-definitions)
(general-define-key
 :definer 'minor-mode
 :states 'normal
 :keymaps 'lsp-mode
 "g d" 'xref-find-definitions
 "g r" 'jester/xref-find-references-at-point)

(provide 'init-jump)
