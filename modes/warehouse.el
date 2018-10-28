  (spacemacs/set-leader-keys-for-major-mode 'erc-mode
    "q" 'erc-quit-server)

  (with-eval-after-load 'helm
    (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-buffers)
    (define-key helm-map (kbd "C-w") 'backward-kill-word))

  (add-hook 'php-mode-hook (lambda () (progn
                                        (electric-indent-local-mode -1)
                                        (modify-syntax-entry ?$ "\_" php-mode-syntax-table))))

(evil-define-key 'insert c-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
(evil-define-key 'insert php-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
(evil-define-key 'insert php-mode-map (kbd "C-j") (lambda () (interactive) (insert " => ")))







;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-flycheck))
;; (require 'lsp-mode)
;; (require 'company-lsp)
;; (add-to-list 'company-backends 'company-lsp)












(defun jester/init-rjsx-mode ()
  (use-package rjsx-mode
    :init
    (setq
     magic-mode-alist (append
                       '(("import\s+.+\s+from\s+['\"]react['\"]" . rjsx-mode))
                       magic-mode-alist))
    (add-hook
     'rjsx-mode-hook
     (lambda () (flycheck-mode 1)
       (evil-matchit-mode 1)
       (add-hook 'post-command-hook 'jester/on-post-newline nil t)
       (setq imenu-create-index-function (lambda () (jester/merge-imenu 'js2-mode-create-imenu-index)))
       (setq
        imenu-generic-expression
        '((nil "^  \\(state\\) = {" 1)))
       ))
    :config
    (evil-define-key 'insert rjsx-mode-map (kbd "C-b") #'rjsx-delete-creates-full-tag)
    (modify-syntax-entry ?_ "w" rjsx-mode-syntax-table)

    (defun jester/import-antd-form-function ()
      "import the function at point as an antd form util function"
      (interactive)
      (let ((fun (thing-at-point 'symbol)))
        (evil-open-above 1)
        (insert (format "const { %s } = this.props.form;" fun))
        (evil-normal-state)))

   ))







;; TODO more prettify symbols
;; TODO evil-collection

