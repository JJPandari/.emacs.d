;; -*- lexical-binding: t -*-
;;----------------------------------------------------------------------------
;; mode line segments
;;----------------------------------------------------------------------------
(defun jester/mode-line-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `(space :align-to
                               (- (+ right right-fringe right-margin) ,reserve))))

(defvar jester/flycheck-mode-line
  '(:eval
    (when (and (featurep 'flycheck) flycheck-mode)
      (pcase flycheck-last-status-change
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

(defvar jester/number-unicode-char-list
  '("⓿" "➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "⓿"))

(setq-default mode-line-format
              (list

               "%1"
               '(:eval (when (and (featurep 'eyebrowse) eyebrowse-mode)
                         (nth (eyebrowse--get 'current-slot) jester/number-unicode-char-list)))

               ;; " %1"
               ;; ;; evil state
               ;; '(:eval evil-mode-line-tag)

               "%1"
               '(:eval (when (buffer-narrowed-p)
                         (propertize " " 'face '(:family "github-octicons"))))

               "%1"
               '(:eval (when defining-kbd-macro
                         (propertize (format " %s%c" "" evil-this-macro) 'face '(:family "FontAwesome"))))

               ;; TODO show count when evil search
               ;; "%1"
               ;; '(:eval (when (or isearch-mode (memq last-command '(evil-search-next evil-search-previous)))
               ;;           (propertize (format " %s/%s" isearch-lazy-count-current isearch-lazy-count-total)
               ;;                       'face 'font-lock-keyword-face)))

               " %1"
               ;; file save stat and buffer name
               '(:eval (propertize "%+ %b"
                                   'face (if (and (buffer-file-name) (buffer-modified-p))
                                             'diff-removed
                                           'font-lock-keyword-face)
                                   'help-echo (buffer-file-name)))

               " %1"
               ;; the current major mode for the buffer.
               '(:eval mode-name)

               " %1"
               jester/flycheck-mode-line

               "%1"
               ;; git info
               '(:eval (when vc-mode
                         (s-replace "Git" (propertize "" 'face '(:family "github-octicons")) (propertize vc-mode))))

               ;; minor modes
               ;; minor-mode-alist

               ;; " "
               ;; global-mode-string (like org) goes in mode-line-misc-info
               ;; mode-line-misc-info

               ;; TODO Error during redisplay:
               ;; (eval (if (boundp (quote org-pomodoro-mode-line)) org-pomodoro-mode-line org-mode-line-string))
               ;; signaled (void-variable org-mode-line-string)
               ;; '(:eval (if (boundp 'org-pomodoro-mode-line) org-pomodoro-mode-line org-mode-line-string))

               '(:eval (jester/mode-line-fill 20))

               ;; column
               '(:eval (propertize " ❚%2c" 'face `(:foreground ,(face-attribute 'font-lock-type-face :foreground))))

               " %1"
               '(:eval (symbol-name buffer-file-coding-system))

               ;; size of file
               '(:eval (propertize " %I" 'face `(:foreground ,(face-attribute 'font-lock-constant-face :foreground))))

               mode-line-end-spaces))


;; (use-package doom-modeline
;;   :ensure t
;;   :custom ((doom-modeline-height 18))
;;   :hook (after-init . doom-modeline-mode))


(provide 'init-mode-line)
