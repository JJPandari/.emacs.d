(setq compilation-read-command nil
      compilation-scroll-output t)
(after-load 'compile
  (require 'ansi-color)
  (defun jester/colourise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'jester/colourise-compilation-buffer)
  (add-hook 'compilation-mode-hook 'jester/set-shell-buffer-face))


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


(defvar-local jester-run-command nil
  "shell command to run the project.")

(defun jester/run-project ()
  "Run it with `compile' and `jester-run-command'."
  (interactive)
  (if jester-run-command
      (compile jester-run-command)
    (user-error "no run command configured for this buffer, check mode hooks"))
  ;; switch to "*compilation*" buffer because it's configured not to show when `compile'
  (switch-to-buffer "*compilation*"))


(defvar-local jester-test-command nil
  "shell command to run the test.")

(defun jester/run-test ()
  "Run it with `compile' and `jester-test-command'."
  (interactive)
  (if jester-test-command
      (compile jester-test-command)
    (user-error "no test command configured for this buffer, check mode hooks"))
  ;; switch to "*compilation*" buffer because it's configured not to show when `compile'
  (switch-to-buffer "*compilation*"))

(jester/with-leader
 ;; TODO assign a key to "cargo check"?
 "c i" 'compile ; "compile it!"
 "c l" (lambda! (switch-to-buffer "*compilation*")) ; "compilation log"
 "c r" 'jester/run-project
 "c t" 'jester/run-test)

(general-define-key
 :states '(motion)
 :keymaps 'compilation-mode-map
 "h" 'evil-backward-char
 "f" 'link-hint-open-link
 "g g" 'evil-goto-first-line
 "g r" 'recompile)


(provide 'init-compile)
