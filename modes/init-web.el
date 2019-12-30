(use-package web-mode
  :mode ("\\.vue\\'" "\\.blade.php\\'" "\\.html\\'" "\\.xml\\'" "\\.xhtml\\'")
  :config
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   ;; "t" 'jester/evil-a-tag-dwim
   "t" 'evil-a-tag)
  (jester/with-major-leader 'web-mode-map
    "r" 'web-mode-element-rename
    "f" 'web-mode-fold-or-unfold
    "e" 'web-mode-element-extract)

  (setq
   web-mode-style-padding 0
   web-mode-script-padding 0
   web-mode-block-padding 0
   web-mode-enable-current-element-highlight t
   web-mode-enable-auto-indentation nil
   web-mode-comment-formats '(("java" . "//") ("javascript" . "//") ("php" . "//")))
  (setq-default
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2)

  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  ;; "-" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  (modify-syntax-entry ?- "w" web-mode-syntax-table)
  (modify-syntax-entry ?# "_" web-mode-syntax-table)
  (modify-syntax-entry ?+ "." web-mode-syntax-table)

  (define-key web-mode-map (kbd "TAB") nil)
  (define-key web-mode-map (kbd "<tab>") nil)
  (evil-define-key 'insert web-mode-map (kbd "TAB") #'tab-indent-or-complete)
  (evil-define-key 'insert web-mode-map (kbd "<tab>") #'tab-indent-or-complete)

  (add-hook! 'web-mode-hook
    (setq imenu-create-index-function (lambda () (jester/merge-imenu 'web-mode-imenu-index)))
    (when (and buffer-file-name (equal (file-name-extension buffer-file-name) "vue"))
      (setq imenu-generic-expression ; imenu regexps for vue.js
            '(("method" "^    \\([^ ]+\\)(.*) {" 1)
              ("data" "^    \\([^ ]+\\): {" 1)
              ("prop" "^  \\([^ ]+\\): {" 1)
              ("hook" "^  \\([^ ]+\\)() {" 1)))
      ;; only use eslint as checker in vue files
      (flycheck-add-mode 'javascript-eslint 'web-mode)) t)

  )

(use-package emmet-mode
  :hook (web-mode js2-mode)
  :config
  (general-define-key
   :states '(insert emacs)
   :keymaps '(web-mode-map js2-mode-map)
   "C-l" 'emmet-expand-yas))

;;----------------------------------------------------------------------------
;; evil text object for html attribute
;;----------------------------------------------------------------------------
(evil-define-text-object jester/evil-web-inner-attribute-or-arg (count &optional beg end type)
  "Select an attribute, or arg, if not in html."
  :extend-selection nil
  (condition-case err
      (list (web-mode-attribute-beginning-position)
            (+ (web-mode-attribute-end-position) 1))
    (error (cl-subseq (jester/evil-inner-arg) 0 2))))

(evil-define-text-object jester/evil-web-a-attribute-or-arg (count &optional beg end type)
  "Select an attribute, including the leading space, or arg, if not in html."
  :extend-selection nil
  ;; when not at an attribute, an error occurs
  (condition-case err
      (list (- (web-mode-attribute-beginning-position) 1)
            (+ (web-mode-attribute-end-position) 1))
    (error (cl-subseq (jester/evil-a-arg) 0 2))))

(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "a" (general-predicate-dispatch 'jester/evil-inner-arg
       (memq major-mode jester-lispy-modes) 'lispyville-inner-atom
       (eq major-mode 'web-mode) 'jester/evil-web-inner-attribute-or-arg))
(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "a" (general-predicate-dispatch 'jester/evil-a-arg
       (memq major-mode jester-lispy-modes) 'lispyville-a-atom
       (eq major-mode 'web-mode) 'jester/evil-web-a-attribute-or-arg))

(provide 'init-web)
