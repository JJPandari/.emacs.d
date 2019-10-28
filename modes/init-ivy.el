(use-package ivy
  :demand t
  :custom
  ((ivy-use-virtual-buffers t)
   (ivy-virtual-abbreviate 'full)
   (ivy-initial-inputs-alist nil)
   (ivy-use-selectable-prompt t)
   (completing-read-function 'ivy-completing-read))
  :config
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "M-k" 'ivy-switch-buffer-kill)
  (jester/with-major-leader '(ivy-occur-mode-map ivy-occur-grep-mode-map wgrep-mode-map)
    "w" (lambda! (ivy-wgrep-change-to-wgrep-mode) (evil-normal-state))
    "," (lambda! (wgrep-finish-edit) (evil-motion-state))
    "a" (lambda! (wgrep-abort-changes) (evil-motion-state))))

(use-package counsel
  :demand t
  :after ivy
  :config
  ;; --no-sort is much faster
  (setq counsel-fzf-cmd "fzf --no-sort -f \"%s\""
        ;; limit file size and line length to be faster. long lines doesn't matter when search but is laggy to display
        counsel-rg-base-command "rg --no-heading --line-number --color never --max-filesize 1M --max-columns 233 --max-columns-preview %s .")

  (jester/with-leader
   "p f" 'jester/open-project-file
   "p i" 'counsel-package
   ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
   "/" 'counsel-rg
   "*" (lambda! (counsel-rg (jester/region-or-symbol)))
   "f r" 'counsel-recentf
   "f z" 'jester/fzf-somewhere
   "t s" 'counsel-load-theme
   "s j" 'counsel-imenu
   "." 'counsel-imenu
   "s b" 'swiper-all
   "s B" (lambda! (swiper-all (jester/region-or-symbol)))
   "i u" 'counsel-unicode-char
   "i f" 'counsel-fonts
   "v s" 'counsel-set-variable)

  (general-define-key
   "M-x" 'counsel-M-x
   "M-y" 'counsel-yank-pop
   "C-s" 'swiper
   "C-S-s" (lambda! (swiper (jester/region-or-symbol))))

  (dolist (that-ivy-map
           '(ivy-mode-map ivy-switch-buffer-map ivy-minibuffer-map
                          counsel-mode-map counsel-describe-map counsel-find-file-map counsel-ag-map
                          swiper-map swiper-all-map))
    (general-define-key
     :keymaps that-ivy-map
     "M-d" #'backward-word
     "M-b" #'kill-word
     "C-w" #'backward-kill-word
     "C-d" #'backward-char
     "C-b" #'delete-char
     "C-v" 'yank
     "C-S-k" 'jester/kill-back-to-indentation
     "H-x" 'kill-region
     "<escape>" #'keyboard-escape-quit))

  (defun jester/fzf-somewhere (&optional start-dir)
    "Do `counsel-fzf' in directory START-DIR.
If called interactively, let the user select start directory first."
    (interactive)
    (unless start-dir
      (ivy-read "dir to start fzf: " #'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :action (lambda (selection) (setq start-dir selection))
                :history 'file-name-history
                :keymap counsel-find-file-map))
    (let ((default-directory start-dir))
      (counsel-fzf)))

  (defun jester/open-project-file ()
    (interactive)
    (cond
     ((locate-dominating-file default-directory ".git") (counsel-git))
     (t (jester/fzf-somewhere))))

  ;; see doc for `counsel-mode'
  (general-define-key
   [remap switch-to-buffer] 'ivy-switch-buffer
   [remap bookmark-jump] 'counsel-bookmark
   [remap describe-bindings] 'counsel-descbinds
   [remap describe-face] 'counsel-describe-face
   [remap describe-function] 'counsel-describe-function
   [remap describe-variable] 'counsel-describe-variable
   [remap execute-extended-command] 'counsel-M-x
   [remap find-file] 'counsel-find-file
   [remap find-library] 'counsel-find-library
   [remap imenu] 'counsel-imenu
   [remap info-lookup-symbol] 'counsel-info-lookup-symbol
   [remap list-faces-display] 'counsel-faces
   [remap load-library] 'counsel-load-library
   [remap load-theme] 'counsel-load-theme
   [remap pop-to-mark-command] 'counsel-mark-ring
   [remap yank-pop] 'counsel-yank-pop))

(use-package wgrep
  :commands ivy-wgrep-change-to-wgrep-mode)


(use-package smex
  :init
  (setq smex-history-length 32)
  :demand t)


;; (use-package ivy-posframe
;;   :demand t
;;   :after ivy
;;   :if window-system
;;   :config
;;   (setq ivy-display-function #'ivy-posframe-display)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;;   ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
;;   (setq ivy-posframe-hide-minibuffer t)
;;   (setq ivy-posframe-parameters
;;         `((background-color . ,(face-attribute 'default :background))
;;           (foreground-color . ,(face-attribute 'default :foreground))))
;;   (ivy-posframe-mode 1))

;;----------------------------------------------------------------------------
;; Pre-fill search keywords
;; https://with-emacs.com/posts/execute-commands-like-marty-mcfly/
;; https://github.com/seagle0128/.emacs.d/blob/d224fae72f672df40b7de0118d1741ddf9b79cf2/lisp/init-ivy.el#L149
;;----------------------------------------------------------------------------
(defvar mcfly-commands
  '(query-replace-regexp
    flush-lines
    keep-lines
    ivy-read
    swiper
    swiper-all
    swiper-isearch
    counsel-grep-or-swiper
    counsel-grep
    counsel-ack
    counsel-ag
    counsel-rg
    counsel-pt))

(defun mcfly-back-to-present ()
  (remove-hook 'pre-command-hook 'mcfly-back-to-present t)
  (cond ((and (memq last-command mcfly-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command '(self-insert-command
                              ivy-yank-word
                              yank))
         (delete-region (point)
                        (point-max)))))

(defun mcfly-time-travel ()
  (when (memq this-command mcfly-commands)
    (let* ((kbd (kbd "M-n"))
           (cmd (key-binding kbd))
           (future (and cmd
                        (with-temp-buffer
                          (when (ignore-errors
                                  (call-interactively cmd) t)
                            (buffer-string))))))
      (when future
        (save-excursion
          (insert (propertize future 'face 'shadow)))
        (add-hook 'pre-command-hook 'mcfly-back-to-present nil t)))))

;; not enabling it cuz it messes with C-s C-s
;; (add-hook 'minibuffer-setup-hook #'mcfly-time-travel)


(provide 'init-ivy)
