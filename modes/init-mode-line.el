(defun jester/mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defvar jester/flycheck-mode-line
;; (setq jester/flycheck-mode-line ;; this is for debug
      '(:eval
        (when (featurep 'flycheck) (pcase flycheck-last-status-change
           (`not-checked nil)
           (`no-checker "❄")
           (`running (propertize "..." 'face 'success))
           (`errored (propertize "!" 'face 'error))
           (`finished
            (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                   (no-errors (cdr (assq 'error error-counts)))
                   (no-warnings (cdr (assq 'warning error-counts)))
                   (face (cond (no-errors 'error)
                               (no-warnings 'warning)
                               (t 'success))))
              (append
               (if no-errors
                   (list
                    (propertize "❌" 'face '(:family "Apple Color Emoji" :height 0.7))
                    (propertize (format "%s" (or no-errors 0))
                                'face 'error))
                 nil)
               (if no-warnings
                   (list
                    (propertize "⚠" 'face '(:family "Apple Color Emoji" :height 0.8))
                    (propertize (format "%s" (or no-warnings 0))
                                'face 'warning))
                 nil))))
           (`interrupted ".")
           (`suspicious '(propertize "?" 'face 'warning))))))

(defvar jester/which-function-mode-line-off-modes '(web-mode scss-mode))

(setq-default mode-line-format
              ;; (setq mode-line-format ;; this is for debug
              (list

               " %1"
               ;; evil state
               '(:eval evil-mode-line-tag)

               " %+"

               ;; anzu
               "%1"
               '(:eval (when (and (featurep 'anzu) anzu--state) (concat " " (anzu--update-mode-line))))

               "%1 "
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               ;; the current major mode for the buffer.
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))

               '(:eval (when (and (featurep 'flycheck) flycheck-mode) " "))
               "%1"
               jester/flycheck-mode-line

               " %1"
               '(:eval (when (featurep 'which-func) (unless (member major-mode jester/which-function-mode-line-off-modes) which-func-format)))

               ;; git info
               '(:eval (when vc-mode
                         (s-replace "Git" (propertize "" 'face '(:family "all-the-icons" :height 1.0)) vc-mode)))

               ;; minor modes
               ;; minor-mode-alist

               ;; " "
               ;; global-mode-string (like org) goes in mode-line-misc-info
               ;; mode-line-misc-info

               ;; TODO Error during redisplay:
               ;; (eval (if (boundp (quote org-pomodoro-mode-line)) org-pomodoro-mode-line org-mode-line-string))
               ;; signaled (void-variable org-mode-line-string)
               ;; '(:eval (if (boundp 'org-pomodoro-mode-line) org-pomodoro-mode-line org-mode-line-string))

               (jester/mode-line-fill 'mode-line 20)

               ;; line and column
               (propertize " ❚%2c" 'face 'font-lock-type-face)

               '(:eval (format " %s" buffer-file-coding-system))

               ;; size of file
               " "
               (propertize "%I" 'face 'font-lock-constant-face) ;; size

               mode-line-end-spaces))

;; TODO  indicator for recording a macro
;; TODO  input method

(provide 'init-mode-line)
