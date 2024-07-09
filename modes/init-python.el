(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-ts-mode)
                ("SConscript\\'" . python-ts-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-ts-mode-hook 'anaconda-mode)
    (add-hook 'python-ts-mode-hook 'anaconda-eldoc-mode))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (add-hook 'python-ts-mode-hook
                (lambda () (jester/local-push-company-backend 'company-anaconda))))))


(provide 'init-python)
