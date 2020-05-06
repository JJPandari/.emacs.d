;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
;; Highlight and allow to open http link at point in programming buffers
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq delete-by-moving-to-trash t)
;; Single space between sentences is more widespread than double
(setq-default sentence-end-double-space nil)

(use-package anzu
  :demand t)
(use-package evil-anzu
  :after anzu
  :demand t)
;; use this in place of anzu when Emacs 27 is here
;; (setq isearch-lazy-count t
;;       lazy-count-prefix-format "%s/%s ")

;; http://emacs.stackexchange.com/a/7745/12854
(defun browse-file-directory ()
  "Open the current file's directory however the OS would."
  (interactive)
  (if default-directory
      (browse-url-of-file (expand-file-name default-directory))
    (error "No `default-directory' to open")))

(jester/with-leader "o d" 'browse-file-directory)

;; re-builder
(general-define-key
 :states '(normal)
 :keymaps 'reb-mode-map
 "q" 'reb-quit)

;; never ring the bell
(setq ring-bell-function 'ignore)

;; make CJK smoother on Windows
(setq inhibit-compacting-font-caches t)

(add-auto-mode 'shell-script-mode "rc\\'")

;; https://emacs-china.org/t/superkey/9387
;; make super key work on Windows
;; (when (eq system-type 'windows-nt) (w32-register-hot-key [s-]))

;; profiler report mode
(general-define-key
 :states 'motion
 :keymaps 'profiler-report-mode-map
 "<tab>" 'profiler-report-toggle-entry
 "<return>" 'profiler-report-toggle-entry
 "j" 'profiler-report-next-entry
 "k" 'profiler-report-previous-entry)


(provide 'init-misc)
