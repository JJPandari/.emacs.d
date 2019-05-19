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

;; don't resize in ediff
(after-load "golden-ratio" (progn
  (add-to-list 'golden-ratio-exclude-modes 'ediff-mode)
  (add-to-list 'golden-ratio-inhibit-functions 'jester/ediff-comparison-buffer-p)))
(defun jester/ediff-comparison-buffer-p ()
  (and (boundp 'ediff-this-buffer-ediff-sessions)
       ediff-this-buffer-ediff-sessions))


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

(use-package shackle
  :demand t
  :config
  (defvar jester-shackle-same-window-rule-cars
    '("*Help*"
      ("^\\*evil-.+\\*$" :regexp t)
      'man-mode
      'woman-mode
      'snippet-mode
      ;; TODO not work
      (nil :custom (lambda (buffer alist plist)
                     (with-current-buffer buffer edit-server-edit-mode))))
    "cars for shackle rule, representing conditions using same-window rule")

  (defvar jester-shackle-same-window-rule
    '(:same t :select t :inhibit-window-quit t)
    "shackle same window rule")

  (setq shackle-default-rule nil
        shackle-rules (append
                       (mapcar (lambda (condition) (append (doom-enlist condition) jester-shackle-same-window-rule))
                               jester-shackle-same-window-rule-cars)
                       '((flycheck-error-list-mode :other t :select t :inhibit-window-quit t))))
  (shackle-mode 1))

;; https://emacs-china.org/t/display-buffer-alist/8162/4?u=jjpandari
;; https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/
;; (setq
;;  display-buffer-alist
;;  '(("^\\*[Hh]elp"
;;     (display-buffer-reuse-window
;;      display-buffer-same-window)
;;     (reusable-frames . visible)
;;     (window-parameters
;;      (select . t)
;;      (quit . nil)
;;      ))))


(defun jester/kill-buffer-and-window ()
  "Kill current buffer and window."
  (interactive)
  (kill-buffer) (delete-window))

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
  "Do golden ratio adjust if it's loaded and enabled."
  (when (and (boundp 'golden-ratio-mode)
             (symbol-value golden-ratio-mode))
    (golden-ratio)))

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
  (jester/maybe-golden-ratio-adjust))

(defun split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-below)
  (windmove-down)
  (jester/maybe-golden-ratio-adjust))


(provide 'init-windows)
