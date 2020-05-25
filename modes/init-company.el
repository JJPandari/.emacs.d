(add-to-list 'completion-styles 'initials t)

(use-package company
  :demand t
  ;; make sure bindings in company-active-map won't be overriden
  :after evil-multiedit
  :init
  (setq company-backends
        '(company-lsp company-capf company-files
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
  ;; M-1 ~ M-0 to select candidate 1 ~ 10
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "M-%d" i)) 'company-complete-number))
  ;; H-1 ~ H-5 to select candidate 6 ~ 10
  (cl-loop for num-key from 1 to 4
           do (let ((candidate-index (+ num-key 5)))
                (define-key company-active-map
                  (read-kbd-macro (format "H-%d" num-key))
                  `(lambda () (interactive) (company-complete-number ,candidate-index)))))
  (define-key company-active-map
    (read-kbd-macro "H-5")
    (lambda () (interactive) (company-complete-number 10)))

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'tab-indent-or-complete
   ))


(use-package company-tabnine
  :demand t
  :config
  (setq jester-company-backends-with-tabnine
        '((company-lsp :with company-tabnine) (company-capf :with company-tabnine)
          company-files
          company-tabnine
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev))
  (defun jester/use-tabnine-for-major-mode (major-mode)
    "add tabnine to `COMPANY-BACKENDS' in `MAJOR-MODE'."
    (add-hook (intern (format "%s-hook" major-mode))
              (lambda () (setq-local company-backends jester-company-backends-with-tabnine)) t))
  (dolist (mode '(js2-mode web-mode typescript-mode css-mode less-css-mode scss-mode))
    (jester/use-tabnine-for-major-mode mode))

  (add-to-list 'company-transformers 'jester/company-merge-tabnine-with-other t)
  (defun jester/company-merge-tabnine-with-other (candidates)
    "Show first 5 of tabnine's candidates, followed by the other backend's candidates.
\"the other\" means company-foo when the group is (company-foo :with company-tabnine)."
    (if (not lsp-mode)
        candidates
      (let ((dedup-table (make-hash-table :test #'equal))
            candidates-other
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate dedup-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-other)
            (puthash candidate t dedup-table)))
        (setq candidates-other (nreverse candidates-other))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 5)
               candidates-other)))))


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
