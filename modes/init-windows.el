;; ;;; -*- lexical-binding: t -*-

;; ;;----------------------------------------------------------------------------
;; ;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;; ;;----------------------------------------------------------------------------
;; (add-hook 'after-init-hook 'winner-mode)


;; 
;; ;; Make "C-x o" prompt for a target window when there are more than 2
;; (require-package 'switch-window)
;; (setq-default switch-window-shortcut-style 'alphabet)
;; (setq-default switch-window-timeout nil)
;; (global-set-key (kbd "C-x o") 'switch-window)


;; ;;----------------------------------------------------------------------------
;; ;; When splitting window, show (other-buffer) in the new window
;; ;;----------------------------------------------------------------------------
;; (defun split-window-func-with-other-buffer (split-function)
;;   (lambda (&optional arg)
;;     "Split this window and switch to the new window unless ARG is provided."
;;     (interactive "P")
;;     (funcall split-function)
;;     (let ((target-window (next-window)))
;;       (set-window-buffer target-window (other-buffer))
;;       (unless arg
;;         (select-window target-window)))))

;; (global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
;; (global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

;; (defun jester/toggle-delete-other-windows ()
;;   "Delete other windows in frame if any, or restore previous window config."
;;   (interactive)
;;   (if (and winner-mode
;;            (equal (selected-window) (next-window)))
;;       (winner-undo)
;;     (delete-other-windows)))

;; (global-set-key (kbd "C-x 1") 'jester/toggle-delete-other-windows)

;; ;;----------------------------------------------------------------------------
;; ;; Rearrange split windows
;; ;;----------------------------------------------------------------------------
;; (defun split-window-horizontally-instead ()
;;   "Kill any other windows and re-split such that the current window is on the top half of the frame."
;;   (interactive)
;;   (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
;;     (delete-other-windows)
;;     (split-window-horizontally)
;;     (when other-buffer
;;       (set-window-buffer (next-window) other-buffer))))

;; (defun split-window-vertically-instead ()
;;   "Kill any other windows and re-split such that the current window is on the left half of the frame."
;;   (interactive)
;;   (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
;;     (delete-other-windows)
;;     (split-window-vertically)
;;     (when other-buffer
;;       (set-window-buffer (next-window) other-buffer))))

;; (global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
;; (global-set-key (kbd "C-x _") 'split-window-vertically-instead)

;; 
;; ;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
;; (defun jester/split-window()
;;   "Split the window to see the most recent buffer in the other window.
;; Call a second time to restore the original window configuration."
;;   (interactive)
;;   (if (eq last-command 'jester/split-window)
;;       (progn
;;         (jump-to-register :jester/split-window)
;;         (setq this-command 'jester/unsplit-window))
;;     (window-configuration-to-register :jester/split-window)
;;     (switch-to-buffer-other-window nil)))

;; (global-set-key (kbd "<f7>") 'jester/split-window)


;; 
;; (defun jester/toggle-current-window-dedication ()
;;   "Toggle whether the current window is dedicated to its current buffer."
;;   (interactive)
;;   (let* ((window (selected-window))
;;          (was-dedicated (window-dedicated-p window)))
;;     (set-window-dedicated-p window (not was-dedicated))
;;     (message "Window %sdedicated to %s"
;;              (if was-dedicated "no longer " "")
;;              (buffer-name))))

;; (global-set-key (kbd "C-c <down>") 'jester/toggle-current-window-dedication)



(setq help-window-select t)
;; important for golden-ratio to better work
(setq window-combination-resize t)


(use-package golden-ratio
  :demand t
  :config
  (golden-ratio-mode 1)
  (jester/with-leader
   "t g" 'golden-ratio-mode)
  (setq golden-ratio-auto-scale t))

(jester/with-leader
 "w s" 'split-window-below
 "w S" 'split-window-below-and-focus
 "w v" 'split-window-right
 "w V" 'split-window-right-and-focus
 "w k" 'delete-window
 "w H" 'evil-window-move-far-left
 "w L" 'evil-window-move-far-right
 "w K" 'evil-window-move-very-top
 "w J" 'evil-window-move-very-bottom
 "w m" 'jester/toggle-maximize-window
 "<tab>" 'jester/alternate-window)

(use-package winner
  :demand t
  :config
  (winner-mode 1)
  (jester/with-leader
   "w u" 'winner-undo
   "w r" 'winner-redo))

(use-package winum
  :demand t
  :config
  (winum-mode 1)
  (jester/with-leader
   "0" 'winum-select-window-0-or-10
   "1" 'winum-select-window-1
   "2" 'winum-select-window-2
   "3" 'winum-select-window-3
   "4" 'winum-select-window-4
   "5" 'winum-select-window-5
   "6" 'winum-select-window-6
   "7" 'winum-select-window-7
   "8" 'winum-select-window-8
   "9" 'winum-select-window-9)
  (advice-add 'winum-select-window-by-number :after (lambda (&optional arg) (jester/maybe-golden-ratio-adjust))))

(use-package ace-window
  :commands ace-swap-window
  :init
  (jester/with-leader
   "w M" 'ace-swap-window))

(defun jester/alternate-window ()
  "Switch back and forth between current and last window in the
current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found."))
    (select-window prev-window)
    (jester/maybe-golden-ratio-adjust)))

(defun jester/maybe-golden-ratio-adjust ()
  "Do `golden-ratio-adjust' if it's bound."
  (when (fboundp 'golden-ratio-adjust) (golden-ratio-adjust 1)))

;; from https://gist.github.com/3402786
(defun jester/toggle-maximize-window ()
  "Maximize window."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(defun split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))


(provide 'init-windows)
