(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . js2-imenu-extras-mode)
  :init
  (setq js2-include-node-externs t
        js-indent-level 2
        js2-global-externs '("$" "jQuery" "jquery" "_")
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-bounce-indent-p nil
        js-indent-align-list-continuation nil
        js-chain-indent nil
        js-switch-indent-offset 2)
  :config
  ;; "_" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)

  ;; (general-define-key
  ;;  :states '(normal)
  ;;  :keymaps 'js2-mode-map
  ;;  "g d" 'js2-jump-to-definition)

  (general-define-key
   :states '(insert emacs)
   :keymaps 'js2-mode-map
   "RET" 'js2-line-break))

;; (setq mode-name (all-the-icons-icon-for-mode major-mode :height 0.8 :v-adjust 0))
(add-hook! 'js2-mode-hook (setq mode-name "JS2"))


(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :config
  (jester/with-major-leader 'js2-mode-map
    "r" (lambda! (counsel-M-x "^js2r- "))))


(use-package js-doc
  :after js2-mode)


(use-package skewer-mode
  :init
  (setq httpd-port 9871)
  :hook js2-mode
  :config
  (jester/with-major-leader 'js2-mode-map
    "e" 'skewer-eval-last-expression)
  (evil-set-initial-state 'skewer-error-mode 'motion))


(use-package rjsx-mode
  :init
  (defun jester/_sx-file-p (file-ext)
    "Check whether current buffer file is a jsx/tsx file.
`FILE-EXT' is file extension, can be \"js\" or \"ts\"."
    (let ((ext (file-name-extension (buffer-file-name))))
      (when (or (string-equal ext file-ext))
        (point-min)
        (while (looking-at "^//")
          (beginning-of-line 2))
        (looking-at (rx (sequence bol
                                  "import"
                                  (1+ (or word "," "{" "}" whitespace))
                                  (or "from 'react'" "from \"react\"")
                                  (optional ";")
                                  eol))))))
  (defun jester/jsx-file-p ()
    "Check whether current buffer file is a jsx file."
    (jester/_sx-file-p "js"))
  (defun jester/tsx-file-p ()
    "Check whether current buffer file is a jsx file."
    (jester/_sx-file-p "ts"))
  (setq magic-mode-alist
        (append '((jester/jsx-file-p . rjsx-mode)) magic-mode-alist))
  (setq magic-mode-alist
        (append '((jester/tsx-file-p . web-mode)) magic-mode-alist))
  (add-hook! 'rjsx-mode-hook
    (add-hook 'post-command-hook 'jester/on-post-newline nil t)
    (setq imenu-create-index-function (lambda () (jester/merge-imenu 'js2-mode-create-imenu-index))
          imenu-generic-expression '((nil "^ *static \\(propTypes\\|defaultProps\\) = {$" 1))
          mode-name "JSX"
          emmet-expand-jsx-className? t))
  (require 'web-mode)
  (custom-set-faces '(rjsx-tag
                      ((t (:inherit web-mode-html-tag-face)))))
  :mode "\\.jsx\\'"
  :config
  (evil-define-key 'insert rjsx-mode-map (kbd "C-b") #'rjsx-delete-creates-full-tag)
  (modify-syntax-entry ?_ "w" rjsx-mode-syntax-table))


(use-package typescript-mode
  :custom (typescript-indent-level 2)
  :init
  (after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'typescript-mode))
  ;; lsp would set checker to lsp, set it back
  ;; lsp and eslint show different errors, using lsp for now...
  ;; `flycheck-add-next-checker'
  ;; (add-hook! :append 'typescript-mode-hook
  ;;   (setq flycheck-checker 'javascript-eslint))
  :mode "\\.ts\\'"
  :config
  ;; (require 'ansi-color)
  ;; (defun colorize-compilation-buffer ()
  ;;   (ansi-color-apply-on-region compilation-filter-start (point-max)))
  ;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  )


(use-package eacl
  :init
  (general-define-key
   "H-," 'eacl-complete-line
   "H-." 'eacl-complete-multiline)
  :commands (eacl-complete-line eacl-complete-multiline))


;; https://emacs-china.org/t/javascript/7860?u=jjpandari
(defun jester/js2r-toggle-object-property-access-style ()
  "Convert the string at point into a template string."
  (interactive)
  (js2r--guard)
  (js2r--wait-for-parse
   (save-excursion
     (let ((node (js2-node-at-point)))
       (if (js2-string-node-p node)
           (let* ((start (js2-node-abs-pos node))
                  (end (+ start (js2-node-len node))))
             (when (memq (char-before start) '(?\[))
               (save-excursion
                 (goto-char (-  end 1)) (delete-char 2)
                 (goto-char (+ start 1)) (delete-char -2) (insert "."))))
         (let* ((start (js2-node-abs-pos node))
                (end (+ start (js2-node-len node))))
           (when (memq (char-before start) '(?.))
             (save-excursion
               (goto-char end) (insert "\']")
               (goto-char start) (delete-char -1) (insert "[\'")))))))))


(defvar jester-monkey-scripts-port "9981"
  "port number where monkey scripts are hosted on local http server.")

(defun jester/copy-monkey-script-url ()
  "Copy current buffer file's http address."
  (interactive)
  (message (kill-new (format "http://localhost:%s/%s" jester-monkey-scripts-port (buffer-name)))))


(defun jester/make-default-evil-markers-for-js ()
  "Make some evil markers to my habit."
  (save-match-data
    ;; i = "import"
    (goto-char (point-min))
    (cl-loop with x
             while (or (when (looking-at "import")
                         (progn (setq x (point))
                                t))
                       (looking-at "//")
                       (and (looking-at "$")
                            (not (looking-at (rx buffer-end)))))
             do (beginning-of-line 2)
             finally (when (not (null x)) (evil-set-marker ?i x)))
    ;; m = "method"
    ;; v = "view"
    (goto-char (point-min))
    (when (and (search-forward "render() {" nil t)
               (search-forward "return (" nil t))
      (evil-set-marker ?v))
    ;; c = "constructor"
    (goto-char (point-min))
    (when (search-forward "constructor(" nil t)
      (evil-set-marker ?c))
    ;; s = "state"
    (goto-char (point-min))
    (when (search-forward "this.state =" nil t)
      (evil-set-marker ?s))))

(add-hook 'js2-mode-hook 'jester/make-default-evil-markers-for-js t)


(defun jester/set-js-ts-test-command ()
  "If file ends with \".test.js\" or \".spec.js\", set `jester-test-command' to \"node node_modules/.bin/jest ...\"."
  (let ((file-name (buffer-file-name)))
    (when (and file-name
               (cl-some (lambda (suffix) (s-suffix-p suffix file-name))
                        '(".test.js" ".spec.js"
                          ".test.jsx" ".spec.jsx"
                          ".test.ts" ".spec.ts"
                          ".test.tsx" ".spec.tsx")))
      (setq jester-test-command (format "(cd %s && node node_modules/.bin/jest %s --collectCoverageOnlyFrom '')" (projectile-project-root) file-name)))))

(add-hook! '(js2-mode-hook typescript-mode-hook web-mode-hook) 'jester/set-js-ts-test-command)


(defun jester/js2-jump-or-citre-jump ()
  "`js2-jump-to-definition' if we can, `citre-jump' if can't or already at import statement."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (let ((case-fold-search nil)) (search-forward "import " nil t)))
      (citre-jump)
    (condition-case err
        (js2-jump-to-definition)
      (error (citre-jump)))))
(general-define-key
 :states '(normal motion)
 :keymaps '(js2-mode-map)
 "g d" 'jester/js2-jump-or-citre-jump
 ;; "g r" 'jester/xref-find-references-at-point
 )


(provide 'init-javascript)
