;; -*- lexical-binding: t -*-
;;----------------------------------------------------------------------------
;; when buffer is not saved, propertize mode line segments with some green color.
;;----------------------------------------------------------------------------
(defun jester/propertize-mode-line-seg-with-save-stat (string &rest properties)
  "Like `propertize', but adds :background to the face prop to indicate file save state.
Note the props supplied for face must use plist-like but not face name."
  (if (and (buffer-file-name) (buffer-modified-p))
      (if-let ((bg-color (face-attribute 'diff-refine-added :background))
               (face-index (-find-index (lambda (elem) (eq elem 'face)) properties)))
          (let* ((plist-index (1+ face-index))
                 (plist (nth plist-index properties))
                 (stuffed-plist (append `(:inherit 'mode-line :background ,bg-color) plist)))
            (setf (nth plist-index properties) stuffed-plist)
            (apply 'propertize string properties))
        (apply 'propertize string (append properties `(face (:inherit 'mode-line :background ,bg-color)))))
    (apply 'propertize string properties)))

(defalias 'jester/propertize-m-l 'jester/propertize-mode-line-seg-with-save-stat)

;;----------------------------------------------------------------------------
;; mode line segments
;;----------------------------------------------------------------------------
(defun jester/mode-line-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (jester/propertize-m-l " "
                         'display `((space :align-to
                                           (- (+ right right-fringe right-margin) ,reserve)))))

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

(defvar jester/which-function-mode-line-off-modes '(web-mode scss-mode css-mode))

(defvar jester/number-unicode-char-list
  '("⓿" "➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "⓿"))

(setq-default mode-line-format
              (list

               "%1"
               '(:eval (jester/propertize-m-l (when (and (featurep 'eyebrowse) eyebrowse-mode)
                                                (nth (eyebrowse--get 'current-slot) jester/number-unicode-char-list))))

               ;; " %1"
               ;; ;; evil state
               ;; '(:eval evil-mode-line-tag)

               "%1"
               '(:eval (when (buffer-narrowed-p)
                         (jester/propertize-m-l " " 'face '(:family "github-octicons"))))

               "%1"
               '(:eval (when defining-kbd-macro
                         (jester/propertize-m-l (format " %s%c" "" evil-this-macro) 'face '(:family "FontAwesome"))))

               "%1"
               ;; '(:eval (if (buffer-modified-p) (propertize "%+" 'face '(:inherit mode-line :background "#FFAB91"))
               ;;           "%+"))
               '(:eval (jester/propertize-m-l " %+"))

               ;; TODO use this in place of anzu when Emacs 27 is here
               ;; (setq isearch-lazy-count t
               ;;       lazy-count-prefix-format "%s/%s ")

               "%1"
               ;; the buffer name; the file name as a tool tip
               '(:eval (jester/propertize-m-l " %b"
                                              'face 'font-lock-keyword-face
                                              'help-echo (buffer-file-name)))

               "%1"
               ;; the current major mode for the buffer.
               '(:eval (jester/propertize-m-l (concat " " mode-name)))

               " %1"
               jester/flycheck-mode-line

               '(:eval (jester/propertize-m-l (when (and (featurep 'which-func) (not (member major-mode jester/which-function-mode-line-off-modes))) " ")))
               "%1"
               '(:eval (jester/propertize-m-l (when (and (featurep 'which-func) (not (member major-mode jester/which-function-mode-line-off-modes))) which-func-format)))

               "%1"
               ;; git info
               '(:eval (when vc-mode
                         (s-replace "Git" (jester/propertize-m-l "" 'face '((:family "github-octicons"))) (jester/propertize-m-l vc-mode))))

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
               '(:eval (jester/propertize-m-l " ❚%2c" 'face 'font-lock-type-face))

               '(:eval (jester/propertize-m-l (format " %s" buffer-file-coding-system)))

               ;; size of file
               '(:eval (jester/propertize-m-l " %I" 'face 'font-lock-constant-face))

               mode-line-end-spaces))


(provide 'init-mode-line)
