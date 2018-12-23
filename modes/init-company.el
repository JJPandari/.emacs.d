(add-to-list 'completion-styles 'initials t)

(use-package company
  :demand t
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
   company-idle-delay 0.3)

  ;; clear default bindings first
  (setq company-active-map (make-sparse-keymap))
  (general-define-key
   :keymaps 'company-active-map
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "<C-m>" 'company-complete-common
   "<tab>" 'expand-snippet-or-complete-selection
   "C-b" 'company-show-doc-buffer
   "C-g" 'company-abort)
  (dotimes (i 10)
    (define-key company-active-map (read-kbd-macro (format "M-%d" i)) 'company-complete-number))

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'tab-indent-or-complete
   ))

(use-package company-posframe
  :if window-system
  :hook (company-mode . company-posframe-mode))

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
