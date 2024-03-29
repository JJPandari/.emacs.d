(when (and (boundp 'server-name) (string= server-name "maid"))
  (use-package keyfreq
    :hook (after-init . keyfreq-mode)
    :config
    (keyfreq-autosave-mode 1)
    (setq keyfreq-excluded-commands '(
                                      ;; self insert and alike
                                      self-insert-command
                                      newline
                                      org-self-insert-command
                                      awesome-pair-space
                                      term-send-raw
                                      ;; cursor moving commands
                                      forward-char
                                      backward-char
                                      next-line
                                      previous-line
                                      evil-a-WORD
                                      evil-append
                                      evil-backward-char
                                      evil-backward-word-begin
                                      evil-change
                                      evil-change-line
                                      evil-complete-next
                                      evil-complete-previous
                                      evil-delete
                                      evil-delete-backward-char-and-join
                                      evil-delete-char
                                      evil-delete-line
                                      evil-emacs-state
                                      evil-end-of-line
                                      evil-escape-emacs-state
                                      evil-escape-insert-state
                                      evil-escape-isearch
                                      evil-escape-minibuffer
                                      evil-escape-motion-state
                                      evil-escape-visual-state
                                      evil-ex
                                      evil-ex-command
                                      evil-ex-completion
                                      evil-ex-delete-backward-char
                                      evil-exit-emacs-state
                                      evil-exit-visual-state
                                      evil-filepath-inner-text-object
                                      evil-filepath-outer-text-object
                                      evil-find-char
                                      evil-find-char-to
                                      evil-first-non-blank
                                      evil-force-normal-state
                                      evil-forward-char
                                      evil-forward-word-begin
                                      evil-forward-word-end
                                      evil-forward-WORD-end
                                      evil-forward-WORD-begin
                                      evil-backward-WORD-begin
                                      evil-backward-WORD-end
                                      evil-goto-definition
                                      evil-goto-first-line
                                      evil-goto-line
                                      evil-goto-mark-line
                                      evil-indent
                                      evil-inner-WORD
                                      evil-inner-double-quote
                                      evil-inner-single-quote
                                      evil-inner-word
                                      evil-insert
                                      evil-join
                                      evil-jump-backward
                                      evil-jump-forward
                                      evil-mc-make-and-goto-next-match
                                      evil-next-line
                                      evil-next-visual-line
                                      evil-normal-state
                                      evil-open-below
                                      evil-paste-after
                                      evil-paste-before
                                      evil-previous-line
                                      evil-previous-visual-line
                                      evil-record-macro
                                      evil-repeat
                                      ;; evil-replace
                                      evil-ret
                                      evil-scroll-page-down
                                      evil-scroll-page-up
                                      evil-search-forward
                                      evil-search-next
                                      evil-search-word-forward
                                      evil-set-marker
                                      evil-substitute
                                      evil-visual-block
                                      evil-visual-char
                                      evil-visual-line
                                      evil-yank
                                      evil-ex-search-next
                                      evil-ex-search-previous
                                      evil-scroll-down
                                      evil-scroll-up
                                      evil-scroll-line-down
                                      evil-scroll-line-up
                                      ranger-prev-file
                                      ranger-next-file
                                      ;; ivy
                                      ivy-done
                                      ivy-next-line
                                      ivy-previous-line
                                      ivy-backward-delete-char
                                      ivy-occur-next-line
                                      ivy-occur-previous-line
                                      ;; other
                                      magit-section-toggle
                                      undo-tree-undo
                                      ;; undo-tree-redo
                                      company-ignore))))

(provide 'init-keyfreq)
