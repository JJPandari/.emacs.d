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
   "M-k" 'ivy-switch-buffer-kill))

(use-package counsel
  :demand t
  :after ivy
  :config
  (jester/with-leader
   "p f" 'jester/open-project-file
   "p F" (lambda! (jester/open-project-file (thing-at-point 'filename)))
   ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
   "/" 'counsel-ag
   "*" (lambda! (counsel-ag (jester/region-or-symbol)))
   "f r" 'counsel-recentf
   "t s" 'counsel-load-theme
   "s j" 'counsel-imenu
   "s b" 'swiper-all
   "s B" (lambda! (swiper-all (jester/region-or-symbol)))
   "i u" 'counsel-unicode-char)

  ;; TODO install wgrep
  (general-define-key
   "M-x" 'counsel-M-x
   "M-y" 'counsel-yank-pop
   "C-s" 'swiper
   "C-S-s" (lambda! (swiper (jester/region-or-symbol)))
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

(use-package smex
  :init
  (setq smex-history-length 32)
  :demand t)

;; (use-package ivy-posframe
;;   :demand t
;;   :if window-system
;;   (setq ivy-display-function #'ivy-posframe-display)
;;   (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
;;   (setq ivy-display-function #'ivy-posframe-display-at-window-center)
;;   (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
;;   (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
;;   (setq ivy-display-function #'ivy-posframe-display-at-point)
;;   (setq ivy-posframe-parameters
;;         '((background-color . "#e6fadb")))
;;   (ivy-posframe-enable))



(provide 'init-ivy)
