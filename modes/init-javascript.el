(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js2-include-node-externs t
        js-indent-level 2
        js2-global-externs '("$" "jQuery" "jquery" "_")
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-bounce-indent-p nil
        js-indent-align-list-continuation nil
        js-chain-indent nil
        js-switch-indent-offset 2)

  ;; "_" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)

  (general-define-key
   :states '(normal)
   :keymaps 'js2-mode-map
   "g d" 'js2-jump-to-definition)

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

;; (maybe-require-package 'typescript-mode)


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
  (defun jester/jsx-file-p ()
    "Check whether current buffer is a jsx file."
    (point-min)
    (while (looking-at "^//")
      (beginning-of-line 2))
    (looking-at (rx (sequence bol
                              "import"
                              (1+ (or word "," "{" "}" whitespace))
                              "from 'react'"
                              (optional ";")
                              eol))))
  (setq magic-mode-alist
        (append '((jester/jsx-file-p . rjsx-mode)) magic-mode-alist))
  (add-hook! 'rjsx-mode-hook
    (add-hook 'post-command-hook 'jester/on-post-newline nil t)
    (setq imenu-create-index-function (lambda () (jester/merge-imenu 'js2-mode-create-imenu-index))
          imenu-generic-expression '((nil "^ *static \\(propTypes\\|defaultProps\\) = {$" 1))
          mode-name "JSX"))
  (custom-set-faces '(rjsx-tag
                      ((t (:inherit web-mode-html-tag-face)))))
  :config
  (evil-define-key 'insert rjsx-mode-map (kbd "C-b") #'rjsx-delete-creates-full-tag)
  (modify-syntax-entry ?_ "w" rjsx-mode-syntax-table))


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

(require 'init-ui5)


(provide 'init-javascript)
