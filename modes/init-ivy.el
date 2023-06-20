(use-package ivy
  :demand t
  :custom
  ((ivy-use-virtual-buffers t)
   (ivy-virtual-abbreviate 'full)
   (ivy-initial-inputs-alist nil)
   (ivy-use-selectable-prompt t)
   (ivy-height 20))
  :config
  (ivy-mode 1)
  (jester/with-leader "r" 'ivy-resume)
  (jester/with-major-leader '(ivy-occur-mode-map ivy-occur-grep-mode-map wgrep-mode-map)
    "w" (lambda! (ivy-wgrep-change-to-wgrep-mode) (evil-normal-state))
    "," (lambda! (wgrep-finish-edit) (evil-motion-state))
    "a" (lambda! (wgrep-abort-changes) (evil-motion-state)))
  (general-define-key
   :states '(motion)
   :keymaps '(ivy-occur-mode-map ivy-occur-grep-mode-map)
   "H-d" 'ivy-occur-delete-candidate
   "g g" 'evil-goto-first-line
   "g r" 'ivy-occur-revert-buffer
   "<return>" 'ivy-occur-press-and-switch
   "f" 'ivy-occur-press)
  (evil-set-initial-state 'ivy-occur-mode 'motion)
  (defun jester/ivy-copy-current-line ()
    "Copy current line in ivy."
    (interactive)
    (kill-new (substring-no-properties
               (ivy-state-current ivy-last) 0 nil))
    (keyboard-escape-quit)))


(use-package swiper
  :demand t
  :after ivy
  :config
  (jester/with-leader
   "s b" (lambda! (swiper-all (jester/region-or-empty)))
   "s B" (lambda! (swiper-all (jester/region-or-symbol))))

  (general-define-key
   "H-s" 'swiper
   "H-S-s" (lambda! (swiper (jester/region-or-symbol)))
   "C-s" 'swiper-isearch)

  (defun jester/maybe-run-ivy-hydra ()
    "Run `hydra-ivy/body' if there is any content in minibuffer."
    (unless (save-excursion (beginning-of-line) (looking-at "$"))
      (hydra-ivy/body)))

  (defmacro jester/make-fuzzy-search-dwim-command (search-cmd)
    "Make a fuzzy search dwim command with the search command `search-cmd'."
    ;; TODO ...
    ())

  (defun jester/swiper-dwim ()
    "If region is not active, just start swiper. If region contain 1 char, grab the symbol as swiper input, otherwise use the region content.
If swiper started with any input, enable ivy-hydra automatically. (so I can h/j/k/l the list)"
    (interactive)
    (if (region-active-p)
        (minibuffer-with-setup-hook 'jester/maybe-run-ivy-hydra
          (swiper (regexp-quote (let* ((beg (region-beginning))
                                       (end (region-end))
                                       (text (progn (deactivate-mark)
                                                    (buffer-substring-no-properties
                                                     beg end))))
                                  (if (= (- end beg) 1)
                                      (thing-at-point 'symbol t)
                                    text)))))
      (swiper)))

  (general-define-key
   :states '(normal motion)
   "/" 'jester/swiper-dwim)

  (defun jester/self-insert-or-search-previous ()
    "Search previous if nothing in the input area, otherwise self insert."
    (interactive)
    (if (save-excursion (beginning-of-line) (looking-at "$"))
        (progn (call-interactively 'previous-complete-history-element)
               (end-of-line))
      (call-interactively 'self-insert-command)))
  (general-define-key
   :keymaps 'swiper-map
   "/" 'jester/self-insert-or-search-previous
   "M-/" (lambda! (insert "/"))))


(use-package counsel
  :demand t
  :after swiper
  :config
  ;; --no-sort is much faster
  (setq counsel-fzf-cmd "fzf --no-sort --exact -f \"%s\""
        ;; https://github.com/hlissner/doom-emacs/issues/3038
        ;; counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never --hidden --max-filesize 1M --max-columns 233 --max-columns-preview %s || true"
        ;; limit file size and line length to be faster. long lines doesn't matter when search but is laggy to display
        counsel-rg-base-command '("rg" "--with-filename" "--no-heading" "--line-number" "--color" "never" "--hidden" "--max-filesize" "1M" "--max-columns" "233" "--max-columns-preview" "%s"))

  (jester/with-leader
   "p f" 'jester/open-project-file
   "p i" 'counsel-package
   ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
   "x" 'counsel-M-x
   "/" 'jester/counsel-rg-dwim
   "*" (lambda! (counsel-rg (jester/region-or-symbol)))
   "f r" 'counsel-recentf
   "f z" 'jester/fzf-somewhere
   "t s" 'counsel-load-theme
   "s j" 'counsel-imenu
   "i u" 'counsel-unicode-char
   "i f" 'counsel-fonts
   "v s" 'counsel-set-variable)

  (general-define-key
   "M-x" 'counsel-M-x
   "M-y" 'counsel-yank-pop)

  (general-define-key
   :states '(normal motion))

  ;; key bindings in ivy popup
  (general-define-key
   :keymaps '(ivy-switch-buffer-map ivy-minibuffer-map
                                    counsel-mode-map counsel-describe-map counsel-find-file-map counsel-ag-map
                                    swiper-map swiper-all-map)
   "M-d" #'backward-word
   "M-b" #'kill-word
   "C-w" #'backward-kill-word
   "C-," #'backward-char
   "C-." #'forward-char
   "<C-i>" 'beginning-of-line-text
   "C-o" 'move-end-of-line
   "S-<backspace>" 'delete-forward-char
   "H-w" 'evil-delete-backward-word
   "H-x" 'kill-region
   "H-c" 'jester/ivy-copy-current-line
   "H-v" 'yank
   "C-S-k" 'jester/kill-back-to-indentation
   "H-r" 'ivy-reverse-i-search
   "H-e" 'hydra-ivy/body
   "<escape>" #'keyboard-escape-quit
   "C-g" #'keyboard-escape-quit
   "<C-return>" 'ivy-call)

  (general-define-key
   :keymaps 'counsel-ag-map
   "/" 'jester/self-insert-or-search-previous
   "M-/" (lambda! (insert "/")))

  (defun jester/counsel-rg-dwim ()
    "If region is not active, just start counsel-rg. If region contain 1 char, grab the symbol as counsel-rg input, otherwise use the region content.
If counsel-rg started with any input, enable ivy-hydra automatically. (so I can h/j/k/l the list)"
    (interactive)
    (if (region-active-p)
        (minibuffer-with-setup-hook 'jester/maybe-run-ivy-hydra
          (counsel-rg (regexp-quote (let* ((beg (region-beginning))
                                           (end (region-end))
                                           (text (progn (deactivate-mark)
                                                        (buffer-substring-no-properties
                                                         beg end))))
                                      (if (= (- end beg) 1)
                                          (thing-at-point 'symbol t)
                                        text)))))
      (counsel-rg)))

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
     ((projectile-project-root) (counsel-git))
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


(use-package ivy-hydra
  :demand t
  :after ivy
  :config
  (defhydra+ hydra-ivy ()
    ("a" nil)
    ("<escape>" keyboard-escape-quit :exit t)
    ("q" keyboard-escape-quit :exit t)))


(use-package wgrep
  :commands ivy-wgrep-change-to-wgrep-mode)


(use-package ivy-posframe
  :demand t
  :after ivy
  :if window-system
  :config
  (setq ivy-posframe-hide-minibuffer t)
  (add-hook! 'load-theme-hook
    (setq ivy-posframe-parameters
          `((background-color . ,(face-attribute 'default :background))
            (foreground-color . ,(face-attribute 'default :foreground))
            (left-fringe . 8))))
  (set-face-attribute 'ivy-posframe-cursor nil :foreground "RoyalBlue")
  (setq ivy-posframe-style 'frame-bottom-left
        ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (swiper-all      . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display)))
  (ivy-posframe-mode 1))


(use-package ivy-rich
  :demand t
  :after counsel
  :config
  (let ((columns (plist-get (cadr ivy-rich-display-transformers-list) :columns)))
    (plist-put (cadr (car columns))
               :width
               60)
    (plist-put (cadr (nth 4 columns))
               :width
               25))
  (ivy-rich-mode 1))


(use-package prescient
  :custom (prescient-aggressive-file-save t)
  :demand t
  :after (ivy counsel)
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :custom ((ivy-prescient-retain-classic-highlighting t)
           (ivy-prescient-enable-filtering nil))
  :init
  ;; FIXME: put this in :custom doesn't work?
  (setq ivy-prescient-sort-commands
        '(:not ivy-switch-buffer swiper swiper-isearch counsel-rg counsel-ag counsel-imenu
               ivy-xref-show-defs ivy-xref-show-xrefs))
  :demand t
  :after (ivy counsel prescient)
  :config
  (ivy-prescient-mode 1))

;; (use-package company-prescient
;;   :demand t
;;   :after (company prescient)
;;   :config
;;   (company-prescient-mode 1))

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
