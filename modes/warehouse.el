


  (require 'eyebrowse)
  (require 'flycheck)

  (desktop-save-mode 1)

  (global-company-mode t)
  (require 'company-posframe)
  (company-posframe-mode 1)
  (push '(company-posframe-mode . nil)
      desktop-minor-mode-table)

  (run-with-idle-timer 30 t (lambda () (desktop-save "~/.emacs.d")))

  ;; adjust regexp make `company-dabbrev' search words like `dabbrev-expand'
  ;; (setq company-dabbrev-char-regexp "[\\.0-9a-z-_'/]")
  ;; search completion from all buffers, not just same mode buffers.
  (setq company-dabbrev-code-other-buffers 'all)
  ;; don't downcase completion result from dabbrev.
  (setq company-dabbrev-downcase nil)

  (golden-ratio-mode t)
  (setq fci-rule-use-dashes t)
  (setq fci-dash-pattern 0.6)
  (fci-mode 1)
  (add-hook 'prog-mode-hook '(lambda ()
                               (electric-pair-mode 1)
                               (which-function-mode 1)
                               (diminish 'evil-snipe-local-mode)
                               (diminish 'projectile-mode);; TODO excluding it doesn't work?
                               (flycheck-mode 1)
                               (paredit-mode 1)
                               ))
  (global-prettify-symbols-mode t)

  (setq make-backup-files nil)
  (require 'auto-save)
  (auto-save-enable)
  (setq auto-save-idle 1)
  (setq auto-save-silent t)
  ;; (setq auto-save-delete-trailing-whitespace t)

  (add-hook 'inferior-scheme-mode '(lambda () (electric-pair-mode 1)))
  (global-auto-revert-mode 1)
  (setq
   aya-persist-snippets-dir "~/.spacemacs.d/snippets"
   golden-ratio-auto-scale t
   initial-frame-alist (quote ((fullscreen . maximized)))
   frame-resize-pixelwise t
   auto-save-default nil
   org-src-fontify-natively t
   org-agenda-files '("~/org")
   auto-mode-alist (append
                    '(("\\.zsh\\'" . shell-script-mode)
                      ("\\.vue\\'" . web-mode)
                      ("\\.blade.php\\'" . web-mode)
                      )
                    auto-mode-alist)
   ;; auto-save-visited-file-name t
   ;; auto-save-interval 300
   scheme-program-name "csi -:c"
   mmm-global-mode 'maybe
   js2-include-node-externs t
   js-indent-level 2
   css-indent-offset 2
   )
  (delete-selection-mode t)
  (setq-default
   truncate-lines t
   ;; indent-tabs-mode t
   tab-width 2
   evil-move-beyond-eol nil
   display-line-numbers-width nil
   )

  (with-eval-after-load 'evil
    ;; if the open tag is the first in its line and the close tag is the last in its,
    ;;   mark the whole lines containing the this tag pair
    ;; else only mark the tag pair
    ;; TODO evil-this-operator
    (evil-define-text-object jester/evil-a-tag-dwim (count &optional beg end type)
      "Select a tag block's whole lines."
      :extend-selection nil
      (let* ((point-list (evil-select-xml-tag beg end type count t))
             (tag-beg (car point-list))
             (tag-end (cadr point-list))
             (line-beg (progn (goto-char tag-beg) (line-beginning-position))))
        (if (and (looking-back "^\s*")
                 (progn (goto-char tag-end) (looking-at "\s*$"))
                 (not (equal (progn (print (car command-history)) (car command-history)) '(evil-surround-delete 116))))
            (evil-range line-beg (line-end-position) 'line)
          point-list)))

    (evil-define-text-object jester/evil-a-attribute (count &optional beg end type)
      "Select an attribute, including the leading space."
      :extend-selection nil
      (list (- (web-mode-attribute-beginning-position) 1)
            (+ (web-mode-attribute-end-position) 1)))
    (evil-define-text-object jester/evil-inner-attribute (count &optional beg end type)
      "Select an attribute."
      :extend-selection nil
      (list (web-mode-attribute-beginning-position)
            (+ (web-mode-attribute-end-position) 1)))
    )

  (defun jester/goto-kill-end (kill-fun forward?)
    "When supplied with a kill function `kill-fun', go to the point the kill function kills to."
    (let* ((old-max (point-max))
           (new-max (progn (funcall kill-fun) (point-max)))
           (kill-end (progn
                       (undo-tree-undo)
                       (funcall (if forward? '+ '-) (point) (abs (- new-max old-max))))))
      (goto-char kill-end)))

  (require 'paredit-extension)

  (defun jester/goto-end-of-sexp ()
    "Go to the end of current expression."
    (interactive)
    (jester/goto-kill-end 'paredit-kill+ t))

  (defun jester/goto-beginning-of-sexp ()
    "Go to the end of current expression."
    (interactive)
    (jester/goto-kill-end 'fontux/paredit-kill-backward nil))

  ;; http://emacs.stackexchange.com/a/20717/12854
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-S-s") 'spacemacs/swiper-region-or-symbol)
  (define-key evil-inner-text-objects-map (kbd "m") 'evilmi-inner-text-object)
  (define-key evil-outer-text-objects-map (kbd "m") 'evilmi-outer-text-object)
  (define-key evil-outer-text-objects-map (kbd "t") 'jester/evil-a-tag-dwim)
  (define-key evil-inner-text-objects-map (kbd "a") 'jester/evil-inner-attribute)
  (define-key evil-outer-text-objects-map (kbd "a") 'jester/evil-a-attribute)
  (define-key evil-normal-state-map (kbd "+") 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "-") 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "C-a") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-scroll-line-down)
  (define-key evil-motion-state-map (kbd "C-j") 'evil-scroll-line-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-scroll-line-up)
  (define-key evil-motion-state-map (kbd "C-k") 'evil-scroll-line-up)
  (define-key evil-normal-state-map (kbd "'") 'evil-goto-mark)
  ;; (define-key evil-normal-state-map (kbd "SPC '") 'evil-use-register)
  ;; (define-key evil-visual-state-map (kbd "SPC '") 'evil-use-register)
  (evil-define-key 'normal js2-mode-map (kbd "g d") #'js2-jump-to-definition)
  (define-key evil-insert-state-map (kbd "C-t") 'evil-execute-in-normal-state)
  (define-key evil-insert-state-map (kbd "C-b") 'delete-char)
  (define-key evil-insert-state-map (kbd "C-o") 'evil-open-below)
  (define-key evil-insert-state-map (kbd "C-S-o") 'evil-open-above)
  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-d") 'backward-char)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line-text)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-w") #'evil-delete-backward-word)
  (define-key evil-insert-state-map (kbd "C-v") #'yank)
  (define-key evil-insert-state-map (kbd "M-d") #'backward-word)
  (define-key evil-insert-state-map (kbd "M-b") #'kill-word)
(define-key evil-insert-state-map (kbd "M-v") #'evil-scroll-down)
(define-key evil-emacs-state-map (kbd "M-v") #'evil-scroll-down)
(define-key evil-insert-state-map (kbd "M-c") #'evil-scroll-up)
(define-key evil-emacs-state-map (kbd "M-c") #'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "<C-backspace>") #'er/expand-region)
  (define-key evil-insert-state-map (kbd "H-v") #'yank-pop)
  (spacemacs/set-leader-keys
    "(" #'backward-up-list
    ")" #'up-list
    "," #'evil-indent
    "b r" #'rename-buffer
    "b k" #'spacemacs/kill-this-buffer
    "n v" #'narrow-to-region
    ;; "g c" #'magit-commit
    ;; "g C" #'magit-clone
    ;; "g p" #'magit-push
    "g d" #'magit-diff-buffer-file
    "g D" #'magit-diff)
(define-key evil-normal-state-map (kbd "SPC TAB") #'spacemacs/alternate-window)
(define-key evil-normal-state-map (kbd "SPC RET") #'spacemacs/alternate-buffer)
  (defun jester/adjust-window () (golden-ratio-adjust 1))
  (advice-add #'spacemacs/alternate-window :after #'jester/adjust-window)
  (define-key evil-normal-state-map (kbd "C-q") 'spacemacs/evil-search-clear-highlight)
  (define-key evil-normal-state-map (kbd "L") 'jester/goto-end-of-sexp)
  (define-key evil-operator-state-map (kbd "L") 'jester/goto-end-of-sexp)
  (define-key evil-normal-state-map (kbd "H") 'jester/goto-beginning-of-sexp)
  (define-key evil-operator-state-map (kbd "H") 'jester/goto-beginning-of-sexp)
  (define-key prog-mode-map (kbd "H-c") 'aya-create)
  (define-key prog-mode-map (kbd "H-e") 'spacemacs/auto-yasnippet-expand)
  (define-key prog-mode-map (kbd "H-w") 'aya-persist-snippet)
  (define-key prog-mode-map (kbd "H-t") 'jester/expand-to-ternary)
  (define-key prog-mode-map (kbd "H-T") 'jester/expand-to-ternary-condensed)
  (evil-define-key 'normal org-mode-map (kbd "g o") #'org-todo)
  (define-key evil-normal-state-map (kbd "M") 'evilmi-jump-items)
  (define-key evil-visual-state-map (kbd "M") 'evilmi-jump-items)
  (define-key evil-operator-state-map (kbd "M") 'evilmi-jump-items)
  (define-key evil-normal-state-map (kbd "C-l") 'avy-goto-word-1)
  (define-key evil-visual-state-map (kbd "C-l") 'avy-goto-word-1)
  (define-key evil-operator-state-map (kbd "C-l") 'avy-goto-word-1)
  (spacemacs/set-leader-keys-for-major-mode 'snippet-mode
    "," 'yas-load-snippet-buffer-and-close
    "l" 'yas-load-snippet-buffer)
  (define-key evil-normal-state-map (kbd "C-h C-k") 'describe-keymap)
  (define-key evil-visual-state-map (kbd "C-h C-k") 'describe-keymap)
  (define-key evil-evilified-state-map (kbd "C-h C-k") 'describe-keymap)
  (define-key evil-normal-state-map (kbd "C-h C-f") 'describe-face)
  (define-key evil-visual-state-map (kbd "C-h C-f") 'describe-face)
  (define-key evil-evilified-state-map (kbd "C-h C-f") 'describe-face)
  (spacemacs/set-leader-keys-for-major-mode 'erc-mode
    "q" 'erc-quit-server)
  (evil-define-key 'insert paredit-mode-map (kbd "(") #'self-insert-command)
  (evil-define-key 'insert paredit-mode-map (kbd "[") #'self-insert-command)

  (define-key evil-normal-state-map (kbd "SPC l") nil)
  (spacemacs/set-leader-keys
    "l l" #'eyebrowse-switch-to-window-config
    "l p" #'eyebrowse-prev-window-config
    "l n" #'eyebrowse-next-window-config
    "l TAB" #'eyebrowse-last-window-config
    "l d" #'eyebrowse-close-window-config
    "l r" #'eyebrowse-rename-window-config
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

  (define-key minibuffer-local-map (kbd "M-d") #'backward-word)
  (define-key minibuffer-local-map (kbd "M-b") #'kill-word)
  (define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)
  (define-key minibuffer-local-map (kbd "C-d") #'backward-char)
  (define-key minibuffer-local-map (kbd "C-b") #'delete-char)
  (define-key minibuffer-local-map (kbd "C-v") #'yank)
  (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)

  (evil-define-key 'insert js2-mode-map (kbd "C-l") #'emmet-expand-yas)
  (evil-define-key 'insert web-mode-map (kbd "C-l") #'emmet-expand-yas)

  ;; https://stackoverflow.com/a/22114743/4788022
  (defun insert-curly-and-go-inside ()
    "Insert {}.
Threat is as function body when from endline before )"
    (interactive)
    (insert " {\n\n}")
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)
    )
  (evil-define-key 'insert prog-mode-map (kbd "<C-return>") 'insert-curly-and-go-inside)

  (defun jester/evil-search-symbol-forward ()
    "Search forward for symbol under point."
    (interactive)
    (evil-ex-search-word-forward 1 t)
    )
  (define-key evil-normal-state-map (kbd "*") 'jester/evil-search-symbol-forward)
  (define-key evil-motion-state-map (kbd "*") 'jester/evil-search-symbol-forward)

  (defun jester/evil-search-symbol-backward ()
    "Search backward for symbol under point."
    (interactive)
    (evil-ex-search-word-backward 1 t)
    )
  (define-key evil-normal-state-map (kbd "#") 'jester/evil-search-symbol-backward)
  (define-key evil-motion-state-map (kbd "#") 'jester/evil-search-symbol-backward)

  (with-eval-after-load 'helm
    (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-buffers)
    (define-key helm-map (kbd "C-w") 'backward-kill-word))

  ;; (setq-default dotspacemacs-configuration-layers '(
  ;;                                                   (auto-completion :variables
  ;;                                                                    auto-completion-tab-key-behavior complete
  ;;                                                                    ))

  (put 'dired-find-alternate-file 'disabled nil)
  (require 'dired)
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

  (with-eval-after-load 'company
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)
    (define-key company-active-map (kbd "C-,") #'company-complete-common)
    (define-key company-active-map (kbd "TAB") #'company-complete-selection)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
    (define-key company-active-map (kbd "C-l") nil)
    (define-key company-active-map (kbd "C-w") nil)
    (define-key company-active-map (kbd "RET") nil)
    (define-key company-active-map (kbd "<return>") nil)
    )

  ;; (with-eval-after-load 'flyspell
  ;;   (define-key flyspell-mode-map (kbd "C-:") #'flyspell-auto-correct-word)
  ;;   )

  ;; (with-eval-after-load 'web-mode
  ;;   (define-key web-mode-map (kbd "TAB") nil)
  ;;   (define-key web-mode-map (kbd "<tab>") nil)
  ;;   (evil-define-key 'insert web-mode-map (kbd "TAB") #'tab-indent-or-complete)
  ;;   (evil-define-key 'insert web-mode-map (kbd "<tab>") #'tab-indent-or-complete)
  ;;   )

  (with-eval-after-load 'emmet-mode
    (define-key emmet-mode-keymap (kbd "TAB") nil)
    (define-key emmet-mode-keymap (kbd "<tab>") nil)
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") #'tab-indent-or-complete)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") #'tab-indent-or-complete)
    (define-key emmet-mode-keymap (kbd "C-j") #'evil-scroll-line-down)
    )

  ;; https://emacs.stackexchange.com/a/7925/12854
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (cond
     ((minibufferp)
      (minibuffer-complete))
     (t
      ;; (indent-for-tab-command)
      (if (and (or (not yas-minor-mode)
                   (null (do-yas-expand)))
               (check-expansion))
          (progn
            (company-manual-begin)
            (if (null company-candidates)
                (progn
                  (company-abort)
                  (hippie-expand nil)
                  ;; (indent-for-tab-command)
                  )))
        ))))

  ;; (defun tab-complete-or-next-field ()
  ;;   (interactive)
  ;;   (if (or (not yas-minor-mode)
  ;;           (null (do-yas-expand)))
  ;;       (if company-candidates
  ;;           (company-complete-selection)
  ;;         (if (check-expansion)
  ;;             (progn
  ;;               (company-manual-begin)
  ;;               (if (null company-candidates)
  ;;                   (progn
  ;;                     (company-abort)
  ;;                     (yas-next-field))))
  ;;           (yas-next-field)))))

  (defun expand-snippet-or-complete-selection ()
    (interactive)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand))
            (company-abort))
        (company-complete-selection)))

  (defun abort-company-or-yas ()
    (interactive)
    (if (null company-candidates)
        (yas-abort-snippet)
      (company-abort)))

  (define-key evil-insert-state-map [tab] 'tab-indent-or-complete)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-indent-or-complete)
  ;; (global-set-key [(control return)] 'company-complete-common)

  (define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)
  (define-key company-active-map (kbd "C-j") nil)
  (define-key company-active-map (kbd "C-k") nil)
  (define-key company-active-map (kbd "C-d") nil)
  (define-key company-active-map (kbd "C-b") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "C-s") nil)
  (define-key company-active-map (kbd "C-/") nil)

  (with-eval-after-load 'yasnippet
    ;; (define-key yas-keymap [tab] 'tab-complete-or-next-field)
    ;; (define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
    (define-key yas-keymap (kbd "TAB") nil)
    (define-key yas-keymap (kbd "<tab>") nil)
    (define-key yas-keymap [(control tab)] 'yas-next-field)
    (define-key yas-keymap (kbd "<return>") #'yas-next-field)
    (define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)
    (define-key yas-keymap (kbd "C-d") nil)
    (define-key yas-keymap (kbd "C-b") 'yas-skip-and-clear-or-delete-char)
    (setq yas-snippet-dirs '("~/.spacemacs.d/snippets"))
    )

  (defun jester/use-small-font ()
    "Use 15px font."
    (interactive)
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 150
                        :weight 'normal
                        :width 'normal)
    (set-frame-parameter nil 'fullscreen 'maximized))

  (defun jester/use-large-font ()
    "Use 18px font."
    (interactive)
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 180
                        :weight 'normal
                        :width 'normal)
    (set-frame-parameter nil 'fullscreen 'maximized))

  (jester/use-large-font)

  ;; (set-frame-font (font-spec
  ;;                  :family "Source Code Pro"
  ;;                  :height 180
  ;;                  :weight 'normal
  ;;                  :width 'normal) nil t)

  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Source Han Sans CN Regular")))
  (set-fontset-font t nil (font-spec :family "Dejavu Sans Mono") nil 'append)

  ;; http://emacs.stackexchange.com/a/7745/12854
  (defun browse-file-directory ()
    "Open the current file's directory however the OS would."
    (interactive)
    (if default-directory
        (browse-url-of-file (expand-file-name default-directory))
      (error "No `default-directory' to open")))
  (spacemacs/set-leader-keys "od" 'browse-file-directory)

  (add-hook 'php-mode-hook (lambda () (progn
                                        (electric-indent-local-mode -1)
                                        (modify-syntax-entry ?$ "\_" php-mode-syntax-table))))
  (add-hook 'python-mode-hook (lambda () (progn
                                           ;; (setq indent-tabs-mode t)
                                           ;; (add-to-list 'company-backends 'company-jedi)
                                           )))
  ;; (with-eval-after-load 'js2-mode
  ;;   (require 'lsp-javascript-typescript))
  (add-hook 'js2-mode-hook (lambda () (progn
                                    ;; (modify-syntax-entry ?_ "\_" js2-mode-syntax-table)
                                    ;; (modify-syntax-entry ?$ "\_" js2-mode-syntax-table)
                                    ;; (flycheck-mode -1)
                                    ;; (lsp-javascript-typescript-enable)
                                    (js2-refactor-mode 1))) t)
  (setq js2-global-externs '("$" "jQuery" "jquery" "_"))
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)

  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map (kbd "RET") 'ivy-switch-buffer)
    (evil-define-key 'insert org-mode-map (kbd "<C-return>") 'org-insert-heading-respect-content)
    ;; (evil-define-key 'insert org-mode-map (kbd "TAB") 'tab-indent-or-complete)
    ;; (evil-define-key 'insert org-mode-map (kbd "C-TAB") 'org-cycle)
    )

  (evil-define-key 'insert c-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
  (evil-define-key 'insert php-mode-map (kbd "C-l") (lambda () (interactive) (insert "->")))
  (evil-define-key 'insert php-mode-map (kbd "C-j") (lambda () (interactive) (insert " => ")))

  ;; (defun evil-global-marker-p (char)
  ;;   "Whether CHAR denotes a global marker."
  ;;   (or (and (>= char ?a) (<= char ?z))
  ;;       (assq char (default-value 'evil-markers-alist))))

  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))
    (evil-adjust-cursor)
    (when (evil-visual-state-p)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))

  (with-eval-after-load 'web-mode
    (setq
     web-mode-style-padding 0
     web-mode-script-padding 0
     web-mode-block-padding 0
     web-mode-enable-current-element-highlight t
     web-mode-enable-auto-indentation nil
     web-mode-comment-formats '(("java" . "//") ("javascript" . "//") ("php" . "//")))
    (setq-default
     web-mode-markup-indent-offset 2
     web-mode-code-indent-offset 2
     web-mode-css-indent-offset 2)
    (modify-syntax-entry ?' "\"" web-mode-syntax-table)
    (modify-syntax-entry ?` "\"" web-mode-syntax-table)
    ;; "-" as word so company completes kabeb-case
    (modify-syntax-entry ?_ "w" web-mode-syntax-table)
    (modify-syntax-entry ?- "w" web-mode-syntax-table)
    (modify-syntax-entry ?# "_" web-mode-syntax-table)
    (define-key web-mode-map (kbd "TAB") nil)
    (define-key web-mode-map (kbd "<tab>") nil)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "t" 'web-mode-attribute-transpose)
    (spacemacs/set-leader-keys-for-major-mode 'web-mode "k" 'web-mode-attribute-kill)
    (evil-define-key 'insert web-mode-map (kbd "TAB") #'tab-indent-or-complete)
    (evil-define-key 'insert web-mode-map (kbd "<tab>") #'tab-indent-or-complete)

    (flycheck-add-mode 'javascript-eslint 'web-mode)

    ;; (add-to-list 'company-backends-web-mode 'company-lsp)
    ;; (require 'lsp-vue)

    (add-hook
     'web-mode-hook
     (lambda ()
       (display-line-numbers-mode 1)
       (setq company-minimum-prefix-length 2 ;; overwrite spacemacs' hook's settings
             web-mode-style-padding 0
             web-mode-script-padding 0
             web-mode-block-padding 0)
       (setq company-backends (cdr company-backends)) ;; company-css & company-html super slow on osx
       ;; the next line causes an error, why did I add it?
       ;; (let ((current-prefix-arg 1)) (call-interactively 'flycheck-disable-checker nil (vector 'javascript-eslint)))
       ;; (lsp-vue-enable)
       (setq imenu-create-index-function (lambda () (jester/merge-imenu 'web-mode-imenu-index)))
       (when (equal (file-name-extension buffer-file-name) "vue")
         (setq
          imenu-generic-expression ; imenu regexps for vue.js
          '(("method" "^    \\([^ ]+\\)(.*) {" 1)
            ("data" "^    \\([^ ]+\\): {" 1)
            ("prop" "^  \\([^ ]+\\): {" 1)
            ("hook" "^  \\([^ ]+\\)() {" 1))))) t)

    )

  (with-eval-after-load 'flycheck (setq flycheck-idle-change-delay 1))

  (with-eval-after-load 'emmet-mode
    (define-key emmet-mode-keymap (kbd "TAB") nil)
    (define-key emmet-mode-keymap (kbd "<tab>") nil)
    (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") #'tab-indent-or-complete)
    (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") #'tab-indent-or-complete)
    (define-key emmet-mode-keymap (kbd "C-j") #'evil-scroll-line-down)
    )

  (spacemacs/set-leader-keys "oe" (lambda () (interactive) (shell-command (concat "csi " buffer-file-name))))

  ;; (server-start)

  (when (require 'edit-server nil t)
    (setq edit-server-new-frame nil)
    (edit-server-start))
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("stackoverflow\\.com" . markdown-mode)))
  (spacemacs/set-leader-keys-for-minor-mode 'edit-server-edit-mode
    "," 'edit-server-done
    "a" 'edit-server-abort)
  (add-hook 'edit-server-start-hook
            (lambda () (visual-line-mode 1)))

  (defun make-cd-for-terminal ()
    "make a cd command for terminal, targeting current buffer file's dir"
    (interactive)
    (let ((file-name (or (buffer-file-name) list-buffers-directory)))
      (if file-name
          (message (kill-new (format "cd %s\n" (file-name-directory file-name))))
        (error "Buffer not visiting a file"))))

  (defun open-emacs-window ()
    "switch to emacs frame"
    (select-frame-set-input-focus (selected-frame)))

  (spacemacs/set-leader-keys "oy" 'make-cd-for-terminal)
  (spacemacs/set-leader-keys "oi" 'ibuffer)
  (spacemacs/set-leader-keys "oa" 'counsel-ag)
  (spacemacs/set-leader-keys "ov" 'yas-visit-snippet-file)
  (spacemacs/set-leader-keys "os" 'yas-new-snippet)

  (with-eval-after-load 'image-mode
    (evil-define-key 'evilified image-mode-map (kbd "p") #'image-previous-file)
    (evil-define-key 'evilified image-mode-map (kbd "n") #'image-next-file)
  )

  (add-hook 'json-mode-hook (lambda () (display-line-numbers-mode 1)))

  (setq auto-revert-check-vc-info t)

  (add-to-list 'evil-evilified-state-modes 'ibuffer-mode)

  ;; (with-eval-after-load 'lsp-mode
  ;;   (require 'lsp-flycheck))
  ;; (require 'lsp-mode)
  ;; (require 'company-lsp)
  ;; (add-to-list 'company-backends 'company-lsp)

  (require 'exec-path-from-shell)
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (use-package youdao-dictionary
    :bind ("C-c y" . youdao-dictionary-search-at-point+)
    :config
    ;; Enable Cache
    (setq url-automatic-caching t)
    ;; Integrate with popwin-el (https://github.com/m2ym/popwin-el)
    ;; (push "*Youdao Dictionary*" popwin:special-display-config)
    ;; Set file path for saving search history
    (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
    ;; Enable Chinese word segmentation support (支持中文分词)
    ;; (setq youdao-dictionary-use-chinese-word-segmentation t)
    )

  (use-package coffee-mode
    :config
    (spacemacs/set-leader-keys-for-major-mode 'coffee-mode "d" 'coffee-mark-defun))

  ;; (require 'ivy-posframe)
  ;; (setq ivy-display-function #'ivy-posframe-display)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-window-bottom-left)
  ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
  ;; (setq ivy-posframe-parameters
  ;;       '((background-color . "#e6fadb")))
  ;; (ivy-posframe-enable)

  (use-package flycheck-posframe
    :ensure t
    :after flycheck
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    (flycheck-posframe-configure-pretty-defaults))

  (use-package magit
    )

  (require 'keyfreq)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(self-insert-command
                                    forward-char
                                    backward-char
                                    next-line
                                    previous-line
                                    evil-a-WORD
                                    evil-append
                                    evil-backward-char
                                    evil-backward-word-begin
                                    evil-change
                                    evil-change-line
                                    evil-complete-next
                                    evil-complete-previous
                                    evil-delete
                                    evil-delete-backward-char-and-join
                                    evil-delete-char
                                    evil-delete-line
                                    evil-emacs-state
                                    evil-end-of-line
                                    evil-escape-emacs-state
                                    evil-escape-insert-state
                                    evil-escape-isearch
                                    evil-escape-minibuffer
                                    evil-escape-motion-state
                                    evil-escape-visual-state
                                    evil-ex
                                    evil-ex-command
                                    evil-ex-completion
                                    evil-ex-delete-backward-char
                                    evil-exit-emacs-state
                                    evil-exit-visual-state
                                    evil-filepath-inner-text-object
                                    evil-filepath-outer-text-object
                                    evil-find-char
                                    evil-find-char-to
                                    evil-first-non-blank
                                    evil-force-normal-state
                                    evil-forward-char
                                    evil-forward-word-begin
                                    evil-forward-word-end
                                    evil-forward-WORD-end
                                    evil-forward-WORD-begin
                                    evil-backward-WORD-begin
                                    evil-backward-WORD-end
                                    evil-goto-definition
                                    evil-goto-first-line
                                    evil-goto-line
                                    evil-goto-mark-line
                                    evil-indent
                                    evil-inner-WORD
                                    evil-inner-double-quote
                                    evil-inner-single-quote
                                    evil-inner-word
                                    evil-insert
                                    evil-join
                                    evil-jump-backward
                                    evil-jump-forward
                                    evil-mc-make-and-goto-next-match
                                    evil-next-line
                                    evil-next-visual-line
                                    evil-normal-state
                                    evil-open-below
                                    evil-paste-after
                                    evil-paste-before
                                    evil-previous-line
                                    evil-previous-visual-line
                                    evil-record-macro
                                    evil-repeat
                                    evil-replace
                                    evil-ret
                                    evil-scroll-page-down
                                    evil-scroll-page-up
                                    evil-search-forward
                                    evil-search-next
                                    evil-search-word-forward
                                    evil-set-marker
                                    evil-substitute
                                    evil-visual-block
                                    evil-visual-char
                                    evil-visual-line
                                    evil-yank
                                    evil-ex-search-next
                                    evil-ex-search-previous
                                    evil-scroll-down
                                    evil-scroll-up
                                    evil-scroll-line-down
                                    evil-scroll-line-up
                                    ivy-done
                                    ivy-next-line
                                    ivy-previous-line
                                    undo-tree-undo
                                    undo-tree-redo))





;;  in layer

(defun jester/init-evil-snipe ()
  (use-package evil-snipe
    :init
    (setq evil-snipe-repeat-keys nil
          evil-snipe-enable-highlight nil)
    :config
    (add-hook 'prog-mode-hook '(lambda ()
                                 (evil-snipe-local-mode 1)
                                 (evil-snipe-override-local-mode 1)
                                 ))

    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "s") #'evil-substitute)
    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "S") #'evil-change-whole-line)

    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "g s") #'evil-snipe-s)
    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "g S") #'evil-snipe-S)
    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "g t") #'evil-snipe-x)
    (evil-define-key 'normal evil-snipe-local-mode-map (kbd "g T") #'evil-snipe-X)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "z" #'evil-snipe-s)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "Z" #'evil-snipe-S)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "x" #'evil-snipe-x)
    ;; (evil-define-key 'visual evil-snipe-local-mode-map "X" #'evil-snipe-X)
    (define-key evil-normal-state-map (kbd "DEL") 'evil-snipe-repeat-reverse)
    ))

(defun jester/init-evil-multiedit ()
  (use-package evil-multiedit
    :init
    ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
    (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
    :config
    (define-key evil-normal-state-map "R" 'evil-multiedit-match-all)
    (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
    (define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-match-symbol-and-next)
    (define-key evil-visual-state-map (kbd "C-n") 'evil-multiedit-match-symbol-and-next)
    (define-key evil-normal-state-map (kbd "C-p") 'evil-multiedit-match-symbol-and-prev)
    (define-key evil-visual-state-map (kbd "C-p") 'evil-multiedit-match-symbol-and-prev)
    (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)
    (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
    (define-key evil-multiedit-state-map (kbd "<return>") 'evil-multiedit-toggle-or-restrict-region)
    ;; (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
    ;; (define-key evil-multiedit-state-map (kbd "M-n") 'evil-multiedit-next)
    ;; (define-key evil-multiedit-state-map (kbd "M-p") 'evil-multiedit-prev)
    ;; (define-key evil-multiedit-insert-state-map (kbd "M-n") 'evil-multiedit-next)
    ;; (define-key evil-multiedit-insert-state-map (kbd "M-p") 'evil-multiedit-prev)
    ))

(defun jester/post-init-ivy ()
  (use-package ivy
    :init
    (setq
     ivy-virtual-abbreviate 'full
     ivy-initial-inputs-alist nil
     ivy-re-builders-alist '((t . ivy--regex-plus))
     )
    :config
    ))

(defun jester/post-init-counsel ()
  (use-package counsel
    :config
    (defun jester/open-project-file (&optional file-name)
      (interactive)
      (cond
       ((locate-dominating-file default-directory ".git") (counsel-git file-name))
       ;; ((projectile-project-p) (projectile-find-file))
       (t (counsel-find-file file-name))))

    (defun jester/open-project-file-auto-symbol ()
      (interactive)
      (funcall 'jester/open-project-file (thing-at-point 'symbol)))

    ;; https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
    (evil-define-key 'normal prog-mode-map (kbd "RET") 'ivy-switch-buffer)
    (evil-define-key 'normal fundamental-mode-map (kbd "RET") 'ivy-switch-buffer)
    ;; (spacemacs/set-leader-keys "bb" 'ivy-switch-buffer)
    ;; (spacemacs/set-leader-keys "fr" 'counsel-recentf)
    (spacemacs/set-leader-keys "pf" 'jester/open-project-file)
    (spacemacs/set-leader-keys "pF" 'jester/open-project-file-auto-symbol)
    (spacemacs/set-leader-keys "og" 'counsel-git-grep)
    ;; (spacemacs/set-leader-keys "ss" 'swiper)
    ;; (spacemacs/set-leader-keys "ry" 'counsel-yank-pop)
    ;; (spacemacs/set-leader-keys "Ts" 'counsel-load-theme)
    ;; (spacemacs/set-leader-keys "sj" 'counsel-imenu)
    ;; (spacemacs/set-leader-keys "sb" 'swiper-all)
    (dolist (key-map
      (list ivy-mode-map ivy-switch-buffer-map
        counsel-describe-map counsel-find-file-map counsel-git-grep-map counsel-mode-map
        swiper-map swiper-all-map))
      (define-key key-map (kbd "M-d") #'backward-word)
      (define-key key-map (kbd "M-b") #'kill-word)
      (define-key key-map (kbd "C-w") #'backward-kill-word)
      (define-key key-map (kbd "C-d") #'backward-char)
      (define-key key-map (kbd "C-b") #'delete-char)
      )
    ))

(defun jester/init-rjsx-mode ()
  (use-package rjsx-mode
    :init
    (setq
     magic-mode-alist (append
                       '(("import\s+.+\s+from\s+['\"]react['\"]" . rjsx-mode))
                       magic-mode-alist))
    (add-hook
     'rjsx-mode-hook
     (lambda () (flycheck-mode 1)
       (evil-matchit-mode 1)
       (add-hook 'post-command-hook 'jester/on-post-newline nil t)
       (setq imenu-create-index-function (lambda () (jester/merge-imenu 'js2-mode-create-imenu-index)))
       (setq
        imenu-generic-expression
        '((nil "^  \\(state\\) = {" 1)))
       ))
    :config
    (evil-define-key 'insert rjsx-mode-map (kbd "C-b") #'rjsx-delete-creates-full-tag)
    (modify-syntax-entry ?_ "w" rjsx-mode-syntax-table)

    (defun jester/import-antd-form-function ()
      "import the function at point as an antd form util function"
      (interactive)
      (let ((fun (thing-at-point 'symbol)))
        (evil-open-above 1)
        (insert (format "const { %s } = this.props.form;" fun))
        (evil-normal-state)))


   ))




dotspacemacs-configuration-layers
'(;; ----------------------------------------------------------------
  ;; Example of useful layers you may want to use right away.
  ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
  ;; `M-m f e R' (Emacs style) to install them.
  ;; ----------------------------------------------------------------
  ;; helm
  ivy
  auto-completion
  ;; better-defaults
  emacs-lisp
  git
  markdown
  ;; neotree
  org
  ;; (shell :variables
  ;;        shell-default-height 30
  ;;        shell-default-position 'bottom)
  ;; spell-checking
  syntax-checking
  version-control
  ;; smex
  ranger
  ;; gtags
  ;; php
  ;; java
  javascript
  html
  autohotkey
  python
  osx
  lua
  yaml
  coffeescript
  ruby
  go
  jjpandari
  jjpandari-ui
  )



   dotspacemacs-additional-packages '(
                                      ;; http://emacs.stackexchange.com/questions/26729/how-to-install-a-package-from-github-to-over-emacs-builtin-one-in-spacemacs
                                      ;; (evil-multiedit :location (recipe :fetcher github :repo "hlissner/evil-multiedit"))
                                      ;; evil-snipe
                                      helm-emmet
                                      ;; tide
                                      evil-ediff
                                      ;; helm-smex
                                      color-theme-sanityinc-tomorrow
                                      solarized-theme
                                      doom-themes
                                      ;; company-jedi
                                      ;; vue-mode
                                      all-the-icons
                                      all-the-icons-dired
                                      smex
                                      keyfreq
                                      lispy
                                      lispyville
                                      edit-server
                                      eyebrowse
                                      exec-path-from-shell
                                      youdao-dictionary
                                      ivy-posframe
                                      flycheck-posframe
                                      company-posframe

                                      lsp-mode
                                      company-lsp
                                      lsp-ui
                                      lsp-javascript-typescript
                                      lsp-vue
                                      )




;; TODO sync snippets/

;; TODO more prettify symbols
;; TODO evil-collection
