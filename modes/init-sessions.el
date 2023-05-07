;;----------------------------------------------------------------------------
;; save a list of open files in ~/.emacs.d/.emacs.desktop
;;----------------------------------------------------------------------------
(setq desktop-dirname "~/.emacs.d")
(require 'desktop)
;; (desktop-save-mode 1)
(defvar jester-auto-save-desktop-timer nil)
(defun jester/toggle-auto-save-desktop ()
  "Toggle auto save desktop."
  (interactive)
  (if jester-auto-save-desktop-timer
      (progn (cancel-timer jester-auto-save-desktop-timer)
             (setq jester-auto-save-desktop-timer nil)
             (message "auto save desktop is OFF."))
    (setq jester-auto-save-desktop-timer
          (run-with-idle-timer 30 t (lambda () (desktop-save desktop-dirname))))
    (message "auto save desktop is ON.")))
(jester/toggle-auto-save-desktop)

(defun jester/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (jester/time-subtract-millis (current-time)
                                            start-time)))))
(advice-add 'desktop-read :around 'jester/desktop-time-restore)

(defun jester/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (jester/time-subtract-millis (current-time)
                                            start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'jester/desktop-time-buffer-create)

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 1000)
(add-hook 'after-init-hook 'savehist-mode)

(push '(company-posframe-mode . nil)
      desktop-minor-mode-table)
;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ido-buffer-history       . 100)
        (ido-last-directory-list  . 100)
        (ido-work-directory-list  . 100)
        (ido-work-file-list       . 100)
        (ivy-history              . 100)
        (magit-read-rev-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list
        evil-markers-alist
        jester/eyebrowse-random-number-seed))

(setq desktop-locals-to-save
      '(evil-markers-alist
        symbol-overlay-keywords-alist))

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)

;;----------------------------------------------------------------------------
;; save cursor place in file
;;----------------------------------------------------------------------------
(use-package saveplace
  :ensure nil
  :straight nil
  :hook (after-init . save-place-mode))


(provide 'init-sessions)
