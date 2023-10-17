;;----------------------------------------------------------------------------
;; bookmark
;;----------------------------------------------------------------------------
;; (defun jester/bookmark-set-auto-name ()
;;   "Do `bookmark-set', if region is active, use the region as bookmark name."
;;   (interactive)
;;   )
;; TODO use the macro for "/"
(jester/with-leader
 "b l" 'counsel-evil-marks
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
 :states '(normal motion)
 :keymaps '(typescript-mode-map web-mode-map js2-mode-map)
 "g d" 'lsp-bridge-find-def
 "g p" 'lsp-bridge-find-def-return
 "g r" 'lsp-bridge-find-references
 "g t" 'lsp-bridge-find-type-def)

(jester/with-major-leader '(typescript-mode-map web-mode-map js2-mode-map)
  "r" 'lsp-bridge-rename)
(jester/with-leader
 "e l" 'lsp-bridge-diagnostic-list
 "e n" 'lsp-bridge-diagnostic-jump-next
 "e p" 'lsp-bridge-diagnostic-jump-prev)


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


;; (use-package citre
;;   :custom ((citre-enable-imenu-integration nil)
;;            (citre-tags-file-names '("tagz" ".tags" "tags")))
;;   :init
;;   (require 'citre-config))

;;----------------------------------------------------------------------------
;; save recent find-def(find-ref ?) targets in a ring, one ring for each project,
;; so they can be looked up with ivy
;;----------------------------------------------------------------------------
(ignore-errors (make-directory (locate-user-emacs-file "var/jester")))
(setq jester-project-symbol-history-file (locate-user-emacs-file "var/jester/project-symbol-history.el"))

(defun jester/read-symbol-history ()
  "Read symbol history from `jester-project-symbol-history-file'."
  (setq jester-project-symbols-map
        (if (file-exists-p jester-project-symbol-history-file)
            (with-temp-buffer
              (insert-file-contents jester-project-symbol-history-file)
              (read (current-buffer)))
          (ht-create))))

(jester/read-symbol-history)

(defun jester/write-symbol-history ()
  "Save `jester-project-symbols-map.'"
  (let ((filename jester-project-symbol-history-file))
    (with-temp-buffer
      (insert ";;; -*- lisp-data -*-\n")
      (let ((print-length nil)
            (print-level nil))
        (pp jester-project-symbols-map (current-buffer)))
      (write-region nil nil filename nil 'silent))))

(defun jester/save-symbol-history (&optional sym-arg)
  "Advice for find def, save current symbol to history."
  (let* ((project-root (projectile-project-root))
         (sym (if sym-arg sym-arg (thing-at-point 'symbol t)))
         (sym-map-in-project (or (ht-get jester-project-symbols-map project-root)
                                 (ht-create))))
    ;; clear text properties
    (set-text-properties 0 (length sym) nil sym)
    (ht-set! sym-map-in-project sym (list (buffer-file-name) (point)))
    (ht-set! jester-project-symbols-map project-root sym-map-in-project)
    (jester/write-symbol-history)))

(advice-add 'xref-find-definitions :before 'jester/save-symbol-history)
(advice-add 'lsp-bridge-find-def :before 'jester/save-symbol-history)

(defun jester/recent-symbol ()
  "Select a recent symbol, goto it (place before find-def)."
  (interactive)
  (let ((sym-map-in-project (ht-get jester-project-symbols-map (projectile-project-root))))
    (ivy-read "recent symbol: "
              (ht-map (lambda (sym _) sym)
                      sym-map-in-project)
              :action (lambda (selected-sym) (let ((place-info (ht-get sym-map-in-project selected-sym)))
                                          (find-file (car place-info))
                                          (goto-char (nth 1 place-info)))))))

(jester/with-leader "s l" 'jester/recent-symbol)


(provide 'init-jump)
