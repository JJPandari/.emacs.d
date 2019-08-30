(use-package web-mode
  :mode ("\\.vue\\'" "\\.blade.php\\'" "\\.html\\'" "\\.xml\\'" "\\.xhtml\\'")
  :config
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   ;; "t" 'jester/evil-a-tag-dwim
   "t" 'evil-a-tag)
  (jester/with-major-leader 'web-mode-map
                            "r" 'web-mode-element-rename
                            "f" 'web-mode-fold-or-unfold)

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
(evil-define-text-object jester/evil-a-attribute (count &optional beg end type)
  "Select an attribute, including the leading space."
  :extend-selection nil
  (list (- (web-mode-attribute-beginning-position) 1)
        (+ (web-mode-attribute-end-position) 1)))

(evil-define-text-object jester/evil-inner-attribute (count &optional beg end type)
  "Select an attribute."
  :extend-selection nil
  (list (web-mode-attribute-beginning-position)
        (+ (web-mode-attribute-end-position) 1)))

(evil-define-text-object jester/evil-a-arg-or-attribute (count &optional beg end type)
  "Select an arg or attribute."
  :extend-selection nil
  (condition-case err
      (cl-subseq (jester/evil-a-attribute) 0 2)
    (error (cl-subseq (jester/evil-a-arg) 0 2))))

(evil-define-text-object jester/evil-inner-arg-or-attribute (count &optional beg end type)
  "Select an arg or attribute."
  :extend-selection nil
  (condition-case err
      (cl-subseq (jester/evil-inner-attribute) 0 2)
    (error (cl-subseq (jester/evil-inner-arg) 0 2))))

(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "a" 'jester/evil-a-arg-or-attribute)
(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "a" 'jester/evil-inner-arg-or-attribute)

(provide 'init-web)
