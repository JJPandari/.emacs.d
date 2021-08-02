;; -*- lexical-binding: t -*-

(add-to-list 'completion-styles 'initials t)

(use-package company
  :demand t
  ;; make sure bindings in company-active-map won't be overriden
  :after evil-multiedit
  :init
  (setq company-backends
        '(company-capf company-files
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
   company-require-match nil
   company-show-numbers t)

  ;; clear default bindings first
  (setq company-active-map (make-sparse-keymap))
  (general-define-key
   :keymaps 'company-active-map
   "<down>" #'company-select-next
   "<up>" #'company-select-previous
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "<C-m>" 'company-complete-common
   "<tab>" 'jester/expand-yas-or-complete-company
   "C-g" 'company-abort
   "<escape>" (lambda! (company-abort) (evil-normal-state)))
  ;; M-0 ~ M-9 to insert number 0 ~ 9
  (dotimes (i 10)
    (define-key company-active-map
      (read-kbd-macro (format "M-%d" i))
      (eval `(lambda! (insert ,(number-to-string i))))))
  ;; H-1 ~ H-5 to select candidate 6 ~ 10
  (cl-loop for num-key from 1 to 4
           do (let ((candidate-index (+ num-key 5)))
                (define-key company-active-map
                  (read-kbd-macro (format "H-%d" num-key))
                  `(lambda () (interactive) (company-complete-number ,candidate-index)))))
  (define-key company-active-map
    (read-kbd-macro "H-5")
    (lambda () (interactive) (company-complete-number 10)))

  ;; 1~0 selects candidates
  ;; https://github.com/abo-abo/oremacs/blob/9c1dd95f52bd6f65313c50c1a85c8bacdde74581/modes/ora-company.el
  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9)))

  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
                          company-candidates)
              (> (string-to-number k)
                 (length company-candidates))
              (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
          (self-insert-command 1)
        (company-complete-number
         (if (equal k "0")
             10
           (string-to-number k))))))

  (defun my-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k))
           (n (if (equal k "0") 10 (string-to-number k))))
      (cond
       ((or (cl-find-if (lambda (s) (string-match re s)) company-candidates)
            (> n (length company-candidates))
            (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
        (self-insert-command 1))

       ((when (eq n 10))
        (company-filter-candidates))

       (t
        (company-complete-number n)))))

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'jester/yas-or-company-or-hippie))


(use-package company-tabnine
  :demand t
  :config
  (setq jester-company-backends-with-tabnine
        '(company-tabnine
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev))
  (defvar jester-tabnine-active-modes
    '(js2-mode web-mode typescript-mode)
    "major modes I would like to use tabnine")
  (defun jester/use-tabnine-for-major-mode (major-mode)
    "add tabnine to `COMPANY-BACKENDS' in `MAJOR-MODE'."
    (add-hook (derived-mode-hook-name major-mode)
              (lambda () (setq-local company-backends jester-company-backends-with-tabnine)) t))
  (dolist (mode jester-tabnine-active-modes)
    (jester/use-tabnine-for-major-mode mode))

  ;; (add-to-list 'company-transformers 'jester/company-merge-tabnine-with-other t)
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
  :custom (company-posframe-quickhelp-delay nil)
  :hook (company-mode . company-posframe-mode))

;;----------------------------------------------------------------------------
;; Make tab do both yas expand and company.
;;----------------------------------------------------------------------------
;; https://emacs.stackexchange.com/a/7925/12854
(defun jester/yas-or-company-or-hippie ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (if (or (not yas-minor-mode)
            (null (jester/yas-expand-no-prompt)))
        (progn
          (company-manual-begin)
          (if (null company-candidates)
              (progn
                (company-abort)
                (hippie-expand nil))))))))

(defun jester/expand-yas-or-complete-company ()
  (interactive)
  (if (or (not yas-minor-mode)
          (null (jester/yas-expand-no-prompt))
          (company-abort))
      (company-complete-selection)))

(defun jester/abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))


(provide 'init-company)
