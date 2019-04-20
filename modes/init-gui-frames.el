;;----------------------------------------------------------------------------
;; Stop C-z from minimizing windows under OS X
;;----------------------------------------------------------------------------
(defun jester/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'jester/maybe-suspend-frame)


;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.
(if *is-a-mac*
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (set-frame-parameter frame 'menu-bar-lines
                                     (if (display-graphic-p frame)
                                         1 0))))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq frame-title-format "看，灰机！ ✈✈✈✈✈✈✈✈✈"
      frame-resize-pixelwise t)
(set-frame-parameter nil 'undecorated t)
;; (set-frame-parameter nil 'fullscreen (cond ((eq window-system 'x) 'fullboth)
;;                                            ((eq window-system 'mac) 'maximized)
;;                                            (t 'maximized)))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(setq default-frame-alist
      `((fullscreen . ,(cond
                        ;; ((eq window-system 'x) 'fullboth)
                        ((eq window-system 'x) 'maximized) ;; wsl favours maximized
                        ((eq window-system 'mac) 'maximized)
                        (t 'maximized)))
        ;; left & top are required on wsl to correctly position
        (left . (+ 0))
        (top . (+ 0))))


(provide 'init-gui-frames)
