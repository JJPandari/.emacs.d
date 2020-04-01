(add-to-list 'completion-styles 'initials t)

(use-package company
  :demand t
  ;; make sure bindings in company-active-map won't be overriden
  :after evil-multiedit
  :init
  (setq company-backends
        '(company-capf company-files company-css
                       (company-dabbrev-code company-gtags company-etags company-keywords)
                       company-dabbrev))
  :config
  (global-company-mode 1)
  (setq
   company-dabbrev-other-buffers 'all
   ;; t means search buffers with same major mode
   company-dabbrev-code-other-buffers t
   company-dabbrev-code-ignore-case nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-minimum-prefix-length 2
   company-idle-delay 0.1
   company-require-match nil)

  ;; clear default bindings first
  (setq company-active-map (make-sparse-keymap))
  (general-define-key
   :keymaps 'company-active-map
   "<down>" #'company-select-next
   "<up>" #'company-select-previous
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "<C-m>" 'company-complete-common
   "<tab>" 'expand-snippet-or-complete-selection
   "C-g" 'company-abort
   "<escape>" (lambda! (company-abort) (evil-normal-state)))
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "M-%d" i)) 'company-complete-number))

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'tab-indent-or-complete
   ))


(use-package company-tabnine
  :demand t
  :config
  (setq jester-company-backends-with-tabnine (cons 'company-tabnine company-backends))
  (defun jester/use-tabnine-for-major-mode (major-mode)
    "add tabnine to `COMPANY-BACKENDS' in `MAJOR-MODE'."
    (add-hook (intern (format "%s-hook" major-mode))
              (lambda () (setq-local company-backends jester-company-backends-with-tabnine))))
  (dolist (mode '(js2-mode web-mode css-mode less-css-mode scss-mode))
    (jester/use-tabnine-for-major-mode mode)))


;; (use-package company-quickhelp
;;   :hook (company-mode . company-quickhelp-mode))


(use-package company-posframe
  :if window-system
  :hook (company-mode . company-posframe-mode))

;;----------------------------------------------------------------------------
;; Make tab do both yas expand and company.
;;----------------------------------------------------------------------------
;; https://emacs.stackexchange.com/a/7925/12854
(defun check-expansion ()
  ;; (save-excursion
  ;;   (if (looking-at "\\_>") t
  ;;     (backward-char 1)
  ;;     (if (looking-at "\\.") t
  ;;       (backward-char 1)
  ;;       (if (looking-at "->") t nil))))
  t)

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    ;; (indent-for-tab-command)
    (if (and (or (not yas-minor-mode)
                 (null (jester/yas-expand-no-prompt)))
             (check-expansion))
        (progn
          (company-manual-begin)
          (if (null company-candidates)
              (progn
                (company-abort)
                (hippie-expand nil)
                ;; (indent-for-tab-command)
                )))))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (jester/yas-expand-no-prompt))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))


(provide 'init-company)
