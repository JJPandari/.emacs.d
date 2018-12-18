(use-package web-mode
  :mode ("\\.vue\\'" "\\.blade.php\\'" "\\.html\\'")
  :config
  (general-define-key
   :keymaps 'evil-outer-text-objects-map
   "t" 'jester/evil-a-tag-dwim
   "a" 'jester/evil-a-attribute)
  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "a" 'jester/evil-inner-attribute)
  (jester/with-major-leader 'web-mode-map
                            "r" 'web-mode-element-rename)

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

  (define-key web-mode-map (kbd "TAB") nil)
  (define-key web-mode-map (kbd "<tab>") nil)
  (evil-define-key 'insert web-mode-map (kbd "TAB") #'tab-indent-or-complete)
  (evil-define-key 'insert web-mode-map (kbd "<tab>") #'tab-indent-or-complete)

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook! 'web-mode-hook
    (setq imenu-create-index-function (lambda () (jester/merge-imenu 'web-mode-imenu-index)))
    (when (equal (file-name-extension buffer-file-name) "vue")
      (setq imenu-generic-expression ; imenu regexps for vue.js
            '(("method" "^    \\([^ ]+\\)(.*) {" 1)
              ("data" "^    \\([^ ]+\\): {" 1)
              ("prop" "^  \\([^ ]+\\): {" 1)
              ("hook" "^  \\([^ ]+\\)() {" 1)))) t)

  )

(use-package emmet-mode
  :hook (web-mode js2-mode)
  :config
  (general-define-key
   :states '(insert emacs)
   :keymaps '(web-mode-map js2-mode-map)
   "C-l" 'emmet-expand-yas))

;;----------------------------------------------------------------------------
;; include line feeds when tag occupy whole lines
;;----------------------------------------------------------------------------
;; TODO evil-this-operator
(evil-define-text-object jester/evil-a-tag-dwim (count &optional beg end type)
  "Select a tag block's whole lines.
if the open tag is the first in its line and the close tag is the last in its,
  mark the whole lines containing the this tag pair
else only mark the tag pair"
  :extend-selection nil
  (let* ((point-list (evil-select-xml-tag beg end type count t))
         (tag-beg (car point-list))
         (tag-end (cadr point-list))
         (line-beg (progn (goto-char tag-beg) (line-beginning-position))))
    (if (and (looking-back "^\s*")
             (progn (goto-char tag-end) (looking-at "\s*$"))
             (not (equal (progn (print (car command-history)) (car command-history)) '(evil-surround-delete 116))))
        (evil-range line-beg (line-end-position) 'line)
      point-list)))

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

(provide 'init-web)
