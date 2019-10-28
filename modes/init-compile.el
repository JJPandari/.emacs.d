(setq compilation-read-command nil
      compilation-scroll-output t)


(require-package 'alert)
(setq alert-default-style (pcase window-system
                            ('x 'x11)
                            ('mac 'osx-notifier)
                            (_ 'message)))

(defun jester/alert-after-compilation-finish (buf result)
  "Use `alert' to report compilation RESULT if BUF is hidden."
  (when (buffer-live-p buf)
    (unless (catch 'is-visible
              (walk-windows (lambda (w)
                              (when (eq (window-buffer w) buf)
                                (throw 'is-visible t))))
              nil)
      (alert (concat "Compilation " result)
             :buffer buf
             :category 'compilation))))

(after-load 'compile
  (add-hook 'compilation-finish-functions
            'jester/alert-after-compilation-finish))


(after-load 'compile
  (require 'ansi-color)
  (defun jester/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'jester/colourise-compilation-buffer))


(defvar-local jester-run-command nil
  "shell command to run the project.")

(defun jester/run-project ()
  "Run it with `compile' and `jester-run-command'."
  (interactive)
  (if jester-run-command
      (compile jester-run-command)
    (user-error "no run command configured for %s" major-mode))
  ;; switch to "*compilation*" buffer because it's configured not to show when `compile'
  (switch-to-buffer "*compilation*"))

(jester/with-leader
 ;; TODO assign a key to "cargo check"?
 "c i" 'compile ;; "compile it!"
 "c l" (lambda! (switch-to-buffer "*compilation*"))
 "c r" 'jester/run-project)

(general-define-key
 :states '(motion)
 :keymaps 'compilation-mode-map
 "f" 'link-hint-open-link)


(provide 'init-compile)
