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
 "w j" 'split-window-below
 "w l" 'split-window-right
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
  :commands (ace-window ace-swap-window)
  :init
  (jester/with-leader
   "w w" 'ace-window
   "w s" 'ace-swap-window))


(use-package shackle
  :demand t
  :config
  (defvar jester-shackle-same-window-rule-cars
    '("*Help*"
      "*Warnings*"
      "*shell*"
      "*Youdao Dictionary*"
      "JsDocTagDescription"
      "*skewer-error*"
      ("^\\*.*build.*\\*$" :regexp t)
      ("^\\*evil-.+\\*$" :regexp t)
      ("-indirect-" :regexp t)
      'Man-mode
      'woman-mode
      'process-menu-mode
      'snippet-mode
      'display-time-world-mode)
    "cars for shackle rule, representing conditions using same-window rule")

  (defvar jester-shackle-same-window-rule
    '(:same t :select t :inhibit-window-quit t)
    "shackle same window rule")

  (setq shackle-default-rule nil
        shackle-rules (append
                       (mapcar (lambda (condition) (append (doom-enlist condition) jester-shackle-same-window-rule))
                               jester-shackle-same-window-rule-cars)
                       '((flycheck-error-list-mode :other t :select t :inhibit-window-quit t)
                         ("*compilation*" :ignore t))))
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


(use-package eyebrowse
  :demand t
  :config
  (eyebrowse-mode 1)
  (jester/with-leader
   "l l" #'eyebrowse-switch-to-window-config
   "l TAB" #'eyebrowse-last-window-config
   "l k" #'eyebrowse-close-window-config
   "l r" #'eyebrowse-rename-window-config
   "l S" #'jester/eyebrowse-duplicate-window-config
   "l 0" #'eyebrowse-switch-to-window-config-0
   "l 1" #'eyebrowse-switch-to-window-config-1
   "l 2" #'eyebrowse-switch-to-window-config-2
   "l 3" #'eyebrowse-switch-to-window-config-3
   "l 4" #'eyebrowse-switch-to-window-config-4
   "l 5" #'eyebrowse-switch-to-window-config-5
   "l 6" #'eyebrowse-switch-to-window-config-6
   "l 7" #'eyebrowse-switch-to-window-config-7
   "l 8" #'eyebrowse-switch-to-window-config-8
   "l 9" #'eyebrowse-switch-to-window-config-9)

  (defun jester/make-eyebrowse-switcher (alias slot key)
    "Make an eyebrowse switch function for SLOT, which has the name/alias of ALIAS,
bound to KEY in the leader sub-keymap."
    (let ((fun-name (intern (format "jester/eyebrowse-switch-to-%s" alias))))
      (eval `(jester/with-leader ,(format "l %s" key) fun-name))
      (eval `(defun ,fun-name ()
               ,(format "Use %s as name/alias for slot %s (if it doesn't already have a name), and switch to it." alias slot)
               (interactive)
               ;; only rename when not already have a name
               (if (eyebrowse--window-config-present-p ,slot)
                   (eyebrowse-switch-to-window-config ,slot)
                 ;; have to switch (implicitly create) first,
                 ;; otherwise rename may operate on a non-existent config, causing error
                 (eyebrowse-switch-to-window-config ,slot)
                 (eyebrowse-rename-window-config ,slot ,alias))))))

  (jester/make-eyebrowse-switcher "alternative" 2 "a")
  (jester/make-eyebrowse-switcher "main" 3 "m")
  (jester/make-eyebrowse-switcher "terms" 4 "t")
  (jester/make-eyebrowse-switcher "scratch" 6 "s")
  (jester/make-eyebrowse-switcher "config" 9 "c")
  (jester/make-eyebrowse-switcher "org" 0 "g"))

;; TODO don't use this count/seed, just try 10000, 10001... till we find an available one
(defvar jester/eyebrowse-random-number-seed 1 "For generating a layout number for `jester/eyebrowse-duplicate-window-config'.")

(defun jester/eyebrowse-duplicate-window-config ()
  "Duplicate current window config, assign a random layout number starting from 10001."
  (interactive)
  (cl-incf jester/eyebrowse-random-number-seed)
  (eyebrowse-switch-to-window-config (+ jester/eyebrowse-random-number-seed 10000)))

;;----------------------------------------------------------------------------
;; scroll smoothly, press scroll easily, scroll to center sometimes...
;;----------------------------------------------------------------------------
(setq scroll-conservatively 999
      scroll-margin 0
      scroll-preserve-screen-position t)

;; when `scroll-conservatively' is 0, recenter happens automatically, not when 999
(add-hook 'counsel-grep-post-action-hook 'recenter)

;; learned from `swiper--maybe-recenter', yay!
(defun jester/maybe-recenter (orig-fun &rest args)
  "If point moved out of the original (before ORIG-FUN runs) window, recenter."
  (let ((prev-window-start (window-start))
        (prev-window-end (window-end)))
    (apply orig-fun args)
    (when (or
           (< (point) prev-window-start)
           (> (point) prev-window-end))
      (recenter))))

(dolist (command '(xref-pop-marker-stack
                   evil-goto-mark evil-goto-last-change evil-goto-last-change-reverse evil-goto-line
                   evil-search-next evil-search-previous
                   ;; backward-up-list up-list ; used by some idle timer, causing my point bouncing everywhere!
                   evilmi-jump-items
                   symbol-overlay-jump-next symbol-overlay-jump-prev
                   flycheck-next-error flycheck-previous-error
                   magit-diff-visit-file
                   jester/recent-symbol))
  (advice-add command :around 'jester/maybe-recenter))

(use-package smooth-scroll
  :demand t
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/hscroll-step-size 4
        smooth-scroll/vscroll-step-size 4)
  (general-define-key
   :states '(normal motion)
   "C-j" 'evil-scroll-line-down
   "C-k" 'evil-scroll-line-up))

(defhydra jester/hydra-scroll ()
  "scroll"
  ("d" evil-scroll-down "down")
  ("u" evil-scroll-up "up"))
(hydra-set-property 'jester/hydra-scroll :verbosity 0)

(jester/with-leader
 "d" 'jester/hydra-scroll/evil-scroll-down
 "u" 'jester/hydra-scroll/evil-scroll-up)

(defhydra jester/hydra-scroll-line ()
  "scroll line"
  ;; the function names ("up" "down") are the opposite from intuition
  ;; scroll-up-1
  ;; scroll-down-1
  ("k" scroll-down-line "up")
  ("j" scroll-up-line "down")
  ;; put "f" in body so (SPC j f) actually runs find-function
  ("f" find-function "(j f) find-function" :exit t)
  ("K" find-function-on-key "(j K) find-function-on-key" :exit t)
  ("v" find-variable "(j v) find-variable" :exit t)
  ("w" which-key-show-major-mode "(k w) which-key-show-major-mode" :exit t))
(hydra-set-property 'jester/hydra-scroll-line :verbosity 0)

(jester/with-leader
 "k" 'jester/hydra-scroll-line/scroll-down-line
 "j" 'jester/hydra-scroll-line/scroll-up-line)


(jester/with-leader
 "w =" 'balance-windows)

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

(defun jester/copy-recent-window ()
  "Use the same buffer as the recent window."
  (interactive)
  (switch-to-buffer (with-selected-window (get-mru-window nil t t)
                      (current-buffer))))

(jester/with-leader
 "w c" 'jester/copy-recent-window)


(provide 'init-windows)
