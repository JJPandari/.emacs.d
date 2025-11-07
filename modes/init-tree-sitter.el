;; parens are taken care by paredit etc., others shall be electric.
(after-init (electric-pair-mode 1)) ;; this is global


(setq treesit-language-source-alist '((javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
                                      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
                                      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                                      (python . ("https://github.com/tree-sitter/tree-sitter-python"))
                                      (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))))

;; need to manually install if not already: M-x treesit-install-language-grammar


(use-package evil-textobj-tree-sitter
  :init
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "f" (general-predicate-dispatch (evil-textobj-tree-sitter-get-textobj "function.inner")
         (memq major-mode jester-lispy-modes) 'lispyville-inner-function))

  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "f" (general-predicate-dispatch (evil-textobj-tree-sitter-get-textobj "function.outer")
         (memq major-mode jester-lispy-modes) 'lispyville-a-function))

  ;; You can also bind multiple items and we will match the first one we can find
  ;; (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  )

;;----------------------------------------------------------------------------
;; ground-up treesit functions
;;----------------------------------------------------------------------------
(defun jester/treesit-get-current-or-parent-node (level)
  "Get current/parent node, `level' 0 means node at point, 1,2,3... means parent, grand-parent..."
  (let ((node (treesit-node-at (point))))
    (while (> level 0)
      (setq node (treesit-node-parent node))
      (setq level (1- level)))
    node))

(defun jester/treesit-test-show-current-or-parent-node ()
  "Print current/parent node, read the level, 0 means node at point, 1,2,3... means parent, grand-parent..."
  (interactive)
  (let* ((n (string-to-number (read-from-minibuffer "parent level (0 is current): " "0")))
         (node (jester/treesit-get-current-or-parent-node n)))
    (message "%s" node)))

(defun akirak-treesit-raise-node ()
  "Replace the parent node with the current node.
This is primarily intended for editing JSX/TSX."
  (interactive)
  (let* ((node (treesit-node-at (point)))
         (start (treesit-node-start node))
         (parent node))
    (while (= start (treesit-node-start parent))
      (setq node parent)
      (setq parent (treesit-node-parent node)))
    (let ((string (buffer-substring (treesit-node-start node)
                                    (treesit-node-end node)))
          (pos (treesit-node-start parent)))
      (delete-region pos (treesit-node-end parent))
      (goto-char pos)
      (save-excursion (insert string)))))

;; TODO slurp, barf, raise "...statement", paredit-kill

(defun jester/wrap-with (begin end string-for-begin string-for-end)
  "Wrap region with given strings."
  (save-excursion
    (goto-char end) ; go to end first, insert at beg would make end oudated
    (insert string-for-end)
    (goto-char begin)
    (insert string-for-begin)))

(defun jester/wrap-round ()
  "Wrap current symbol or region with parens."
  (interactive)
  (-let (((beg . end) (if (region-active-p)
                          (car (region-bounds))
                        (bounds-of-thing-at-point 'symbol))))
    (when (region-active-p)
      (deactivate-mark)
      (goto-char beg))
    (jester/wrap-with beg end "(" ")")))

(general-define-key
 :states '(emacs insert normal visual)
 "H-f" 'jester/wrap-round)

;; TODO append \n to make it evil style
(defun jester/semantic-kill-maybe-whole-line ()
  "Kill semantic unit after point, if only whitespace is left afterwards, delete this line."
  (interactive)
  (awesome-pair-kill)
  (when (and (looking-back "^\s*") (looking-at "\s*$"))
    (delete-region (point) (line-end-position 0))))

(general-define-key
 :states '(normal)
 "d" (general-key-dispatch 'evil-delete
       "l" 'jester/semantic-kill-maybe-whole-line)
 "c" (general-key-dispatch 'evil-change
       "l" (lambda! (awesome-pair-kill) (evil-insert-state))))
;; binding to normal state implicitly binds to visual state, gotta fix it.
(general-define-key
 :states '(visual)
 "d" 'evil-delete
 "c" 'evil-change)


(provide 'init-tree-sitter)
