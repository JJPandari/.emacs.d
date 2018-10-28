;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(add-to-list 'completion-styles 'initials t)

(use-package company
  :demand t
  :config
  (global-company-mode 1)
  (setq
   ;; search completion from all buffers, not just same mode buffers.
   company-dabbrev-code-other-buffers 'all
   company-minimum-prefix-length 2)

  (general-define-key
   :keymaps 'company-active-map
   "M-n" nil
   "M-p" nil
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-," #'company-complete-common
   "C-l" nil
   "C-w" nil
   ;; TODO C-m complete common?
   "RET" nil
   "<return>" nil

   "<tab>" 'expand-snippet-or-complete-selection
   "C-j" nil
   "C-k" nil
   "C-d" nil
   "C-b" 'company-show-doc-buffer
   "C-s" nil
   "C-/" nil
   )

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'tab-indent-or-complete
   ))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode)
  :config
  )

;;----------------------------------------------------------------------------
;; Make tab do both yas expand and company.
;;----------------------------------------------------------------------------
;; https://emacs.stackexchange.com/a/7925/12854
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas-fallback-behavior 'return-nil))
    (yas-expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    ;; (indent-for-tab-command)
    (if (and (or (not yas-minor-mode)
                 (null (do-yas-expand)))
             (check-expansion))
        (progn
          (company-manual-begin)
          (if (null company-candidates)
              (progn
                (company-abort)
                (hippie-expand nil)
                ;; (indent-for-tab-command)
                )))
      ))))

;; (defun tab-complete-or-next-field ()
;;   (interactive)
;;   (if (or (not yas-minor-mode)
;;           (null (do-yas-expand)))
;;       (if company-candidates
;;           (company-complete-selection)
;;         (if (check-expansion)
;;             (progn
;;               (company-manual-begin)
;;               (if (null company-candidates)
;;                   (progn
;;                     (company-abort)
;;                     (yas-next-field))))
;;           (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))






(provide 'init-company)
