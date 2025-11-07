;; TODO in ts & tsx file, electric "<" when last symbol starts with Upper case (probably type)
(use-package web-mode
  :init
  (after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'xml-xmllint 'web-mode)
    (flycheck-add-mode 'xml-xmlstarlet 'web-mode))
  :mode ("\\.tsx\\'" "\\.vue\\'"
         "\\.html\\'" "\\.xml\\'" "\\.xhtml\\'"
         "\\.blade.php\\'" "\\.jsp\\'")
  :config
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   ;; "t" 'jester/evil-a-tag-dwim
   "t" 'evil-a-tag)
  (jester/with-major-leader 'web-mode-map
    "r" 'web-mode-element-rename
    "z" 'web-mode-fold-or-unfold
    "e" 'web-mode-element-extract)

  (setq
   web-mode-style-padding 0
   web-mode-script-padding 0
   web-mode-block-padding 0
   web-mode-enable-current-element-highlight nil
   web-mode-enable-auto-indentation nil
   web-mode-comment-formats '(("java" . "//")
                              ("javascript" . "//")
                              ("typescript" . "//")
                              ("jsx" . "//")
                              ("php" . "//")
                              ("css" . "/*")))
  (setq-default
   web-mode-markup-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2)

  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  (modify-syntax-entry ?_ "_" web-mode-syntax-table)
  (modify-syntax-entry ?- "_" web-mode-syntax-table)
  (modify-syntax-entry ?# "_" web-mode-syntax-table)
  (modify-syntax-entry ?+ "." web-mode-syntax-table)
  (modify-syntax-entry ?| "." web-mode-syntax-table)

  (add-hook 'web-mode-hook 'jester/web-mode-maybe-setup-vue)
  (defun jester/web-mode-maybe-setup-vue ()
    "Do something if it's a .vue file."
    (when (string-equal (file-name-extension (buffer-name)) "vue")
      (setq imenu-create-index-function (lambda () (jester/merge-imenu 'web-mode-imenu-index)))
      (setq imenu-generic-expression ; imenu regexps for vue.js
            '(("method" "^    \\([^ ]+\\)(.*) {" 1)
              ("data" "^    \\([^ ]+\\): {" 1)
              ("prop" "^  \\([^ ]+\\): {" 1)
              ("hook" "^  \\([^ ]+\\)() {" 1)))))

  (add-hook 'web-mode-hook 'jester/web-mode-maybe-setup-tsx)
  (defun jester/web-mode-maybe-setup-tsx ()
    "Do something if it's a .tsx file."
    (when (string-equal (file-name-extension (buffer-name)) "tsx")
      ;; (lsp-diagnostics--enable)
      ;; (flycheck-add-next-checker 'javascript-eslint 'lsp)
      ;; (lsp)
      ;; ;; lsp sets checker to lsp, set it back
      ;; ;; `flycheck-add-next-checker'
      ;; (setq flycheck-checker 'javascript-eslint)
      (jester/make-default-evil-markers-for-js)
      (setq-local emmet-expand-jsx-className? t
                  web-mode-auto-quote-style 3)
      (aggressive-indent-mode -1)))

  (add-hook 'web-mode-hook 'jester/web-mode-maybe-setup-xml)
  (defun jester/web-mode-maybe-setup-xml ()
    "Do something if it's a .xml file."
    (when (string-equal (file-name-extension (buffer-name)) "xml")
      (flycheck-select-checker 'xml-xmllint))))

(use-package emmet-mode
  :custom (emmet-self-closing-tag-style " /")
  :hook (js2-mode web-mode typescript-ts-mode css-mode less-css-mode scss-mode sass-mode)
  :config
  (general-define-key
   :states '(insert emacs)
   :keymaps '(web-mode-map js2-mode-map typescript-ts-mode-map css-mode-map sass-mode-map)
   "H-e" 'emmet-expand-line))

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

;;----------------------------------------------------------------------------
;; enable treesit
;;----------------------------------------------------------------------------
(defun jester/web-mode-maybe-enable-tsx-treesit ()
  "Enable treesit if it's a tsx file."
  (when (string-suffix-p "\.tsx" buffer-file-name)
    (require 'evil-textobj-tree-sitter)
    (add-to-list 'evil-textobj-tree-sitter-major-mode-language-alist '(web-mode . "tsx"))

    ;; evil-textobj only want to work in *-ts-mode, override it
    (defun evil-textobj-tree-sitter--use-builtin-treesitter ()
      "Return non-nil if we should use builtin treesitter."
      (and evil-textobj-tree-sitter--can-use-builtin-treesit
           (let ((major-mode-name (symbol-name major-mode)))
             (or (string-suffix-p "-ts-mode" major-mode-name)
                 (string-equal major-mode-name "web-mode") ; allow web-mode here
                 (and (boundp 'rust-mode-treesitter-derive)
                      rust-mode-treesitter-derive
                      (string= "rustic-mode" major-mode-name))))))

    (treesit-parser-create 'tsx)))

(add-hook 'web-mode-hook 'jester/web-mode-maybe-enable-tsx-treesit)


(provide 'init-web)
