;;----------------------------------------------------------------------------
;; bookmark
;;----------------------------------------------------------------------------
(jester/with-leader
 "b n" 'bookmark-set
 "b j" 'bookmark-jump)

;;----------------------------------------------------------------------------
;; xref
;;----------------------------------------------------------------------------
(defun jester/xref-find-references-at-point ()
  "Find references for symbol at point."
  (interactive)
  (xref-find-references (xref-backend-identifier-at-point (xref-find-backend))))

(general-define-key
 :states '(normal motion)
 "g d" 'xref-find-definitions
 "g p" 'xref-pop-marker-stack)
(general-define-key
 :definer 'minor-mode
 :states 'normal
 :keymaps 'lsp-mode
 "g r" 'jester/xref-find-references-at-point)


;; TODO xref-ring
;; (advice-add 'xref-find-definitions :after )


(use-package dumb-jump
  :init
  (setq dumb-jump-aggressive t
        dumb-jump-selector 'ivy)
  ;; sets the global value, will be overwritten by major mode / lsp
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


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


(use-package citre
  :custom ((citre-enable-imenu-integration nil)
           (citre-update-tags-file-when-no-definitions nil)
           (citre-tags-files '("tagz" ".tags" "tags")))
  :init
  (require 'citre-config))
;; TODO try `citre-peek'


(provide 'init-jump)
