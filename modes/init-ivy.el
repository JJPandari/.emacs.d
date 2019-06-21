(use-package ivy
  :demand t
  :config
  (setq
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'full
   ivy-initial-inputs-alist nil
   ivy-use-selectable-prompt t
   completing-read-function 'ivy-completing-read)
  (general-define-key
   :keymaps 'ivy-switch-buffer-map
   "M-k" 'ivy-switch-buffer-kill)
  ;; TODO
  (jester/with-major-leader 'ivy-occur-mode-map
                            "w" 'ivy-wgrep-change-to-wgrep-mode
                            "," 'wgrep-finish-edit
                            "a" 'wgrep-exit))

(use-package counsel
  :demand t
  :after ivy
  :config
  ;; not needed, ag always respects ~/.agignore (right?)
  ;; (setq counsel-ag-base-command "ag --nocolor --nogroup -p ~/.agignore %s")
  (jester/with-leader
   "p f" 'jester/open-project-file
   "p F" (lambda! (jester/open-project-file (thing-at-point 'filename)))
   ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
   "/" 'counsel-ag
   "*" (lambda! (counsel-ag (jester/region-or-symbol)))
   "f r" 'counsel-recentf
   "t s" 'counsel-load-theme
   "s j" 'counsel-imenu
   "." 'counsel-imenu
   "s b" 'swiper-all
   "s B" (lambda! (swiper-all (jester/region-or-symbol)))
   "i u" 'counsel-unicode-char)

  (general-define-key
   "M-x" 'counsel-M-x
   "M-y" 'counsel-yank-pop
   "C-s" 'swiper
   "C-S-s" (lambda! (swiper (regexp-quote (jester/region-or-symbol))))
   "C-h C-f" 'counsel-describe-face)

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

  (defun jester/open-project-file (&optional file-name)
    (interactive)
    (cond
     ((locate-dominating-file default-directory ".git") (counsel-git file-name))
     (t (counsel-find-file file-name))))

  (general-define-key
   [remap switch-to-buffer] 'ivy-switch-buffer
   [remap bookmark-jump] 'counsel-bookmark
   [remap describe-bindings] 'counsel-descbinds
   [remap describe-face] 'counsel-describe-face
   [remap describe-function] 'counsel-describe-function
   [remap describe-variable] 'counsel-describe-variable
   [remap find-file] 'counsel-find-file
   [remap find-library] 'counsel-find-library
   [remap imenu] 'counsel-imenu
   [remap info-lookup-symbol] 'counsel-info-lookup-symbol
   [remap list-faces-display] 'counsel-faces
   [remap load-library] 'counsel-load-library
   [remap load-theme] 'counsel-load-theme
   [remap pop-to-mark-command] 'counsel-mark-ring))

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
;;   (ivy-posframe-enable))

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
