;;----------------------------------------------------------------------------
;; fix C-m, C-i
;;----------------------------------------------------------------------------
(when window-system
  (define-key input-decode-map [?\C-m] [C-m])
  (define-key input-decode-map [?\C-i] [C-i]))

;;----------------------------------------------------------------------------
;; Config general.
;;----------------------------------------------------------------------------
(use-package general
  :demand t
  :config
  (defconst jester-leader "SPC" "Leader key in normal state.")
  (defconst jester-leader-emacs "M-m" "Leader key in emacs state.")
  (defconst jester-mode-leader "," "Mode specific leader key in normal state.")
  (defconst jester-mode-leader-emacs "M-M" "Major-leader key in emacs state.")

  ;; https://github.com/noctuid/evil-guide#preventing-certain-keys-from-being-overridden
  (general-override-mode)

  (general-define-key :states '(normal motion visual) jester-mode-leader nil)
  (general-define-key :states '(insert emacs) jester-mode-leader-emacs nil)

  (general-create-definer jester/leader-def :keymaps 'override
    :prefix jester-leader :states '(normal motion visual))
  (general-create-definer jester/leader-emacs-def :keymaps 'override
    :prefix jester-leader-emacs :states '(insert emacs))
  (general-create-definer jester/major-leader-def
    :prefix jester-mode-leader :states '(normal motion visual))
  (general-create-definer jester/major-leader-emacs-def
    :prefix jester-mode-leader-emacs :states '(insert emacs))
  (general-create-definer jester/minor-leader-def :definer 'minor-mode
    :prefix jester-mode-leader :states '(normal motion visual))
  (general-create-definer jester/minor-leader-emacs-def :definer 'minor-mode
    :prefix jester-mode-leader-emacs :states '(insert emacs))

  (defmacro jester/with-leader (&rest args)
    "Define a leader key sequence."
    `(progn (jester/leader-def ,@args)
            (jester/leader-emacs-def ,@args)))
  (defmacro jester/with-major-leader (keymaps &rest args)
    "Define a major-leader key sequence."
    `(progn (jester/major-leader-def :keymaps ,keymaps ,@args)
            (jester/major-leader-emacs-def :keymaps ,keymaps ,@args)))
  (defmacro jester/with-minor-leader (mode &rest args)
    "Define a leader key sequence using major leader key, but for a minor mode."
    `(progn (jester/minor-leader-def :keymaps ,mode ,@args)
            (jester/minor-leader-emacs-def :keymaps ,mode ,@args)))
  )

;;----------------------------------------------------------------------------
;; Config evil.
;;----------------------------------------------------------------------------
(use-package evil
  :demand t
  :custom
  (evil-want-Y-yank-to-eol t "Y yanks to eol, not whole line")
  (evil-want-visual-char-semi-exclusive t "don't include line feed in visual select")
  :init
  :config
  (evil-mode 1)
  ;; move by symbol rather than word
  ;; http://emacs.stackexchange.com/a/20717/12854
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-shift-width 2)
  (setq
   evil-move-beyond-eol nil
   evil-normal-state-tag   (propertize "N" 'face '((:background "DarkGoldenrod2" :foreground "black")))
   evil-motion-state-tag   (propertize "M" 'face '((:background "plum3") :foreground "white"))
   evil-visual-state-tag   (propertize "V" 'face '((:background "gray" :foreground "black")))
   evil-insert-state-tag   (propertize "I" 'face '((:background "chartreuse3") :foreground "white"))
   evil-emacs-state-tag    (propertize "E" 'face '((:background "SkyBlue2" :foreground "black")))
   evil-operator-state-tag (propertize "O" 'face '((:background "#ffec8b" :foreground "#93a1a1")))
   evil-normal-state-cursor '("DarkGoldenrod2" box)
   evil-motion-state-cursor '("plum3" box)
   evil-visual-state-cursor '("gray" (hbar . 2))
   evil-insert-state-cursor '("chartreuse3" (bar . 2))
   evil-emacs-state-cursor '("SkyBlue2" box)
   evil-replace-state-cursor '("chocolate" (hbar . 2)))

  ;; TODO not working
  (evil-set-command-property 'evil-shift-left :keep-visual t)

  (evil-define-text-object jester/evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'jester/evil-inner-buffer)

  (evil-define-operator jester/evil-join-no-whitespace (beg end)
    "Join lines without whitespace."
    :motion evil-line
    (let ((count (count-lines beg end)))
      (when (> count 1)
        (setq count (1- count)))
      (goto-char beg)
      (dotimes (var count)
        (let ((del-beg (progn (evil-move-end-of-line 1) (point)))
              (del-end (progn (beginning-of-line-text 2) (point))))
          (delete-region del-beg del-end)))))

  (general-define-key
   :states '(normal)
   :keymaps '(evil-command-window-mode-map)
   "q" (lambda! (jester/kill-buffer-and-window) (select-window (previous-window))))

  )


(use-package evil-exchange
  :demand t
  :config
  (evil-exchange-install))


(use-package evil-nerd-commenter
  :demand t
  :config
  (jester/with-leader
   ";" 'evilnc-comment-operator))


(use-package evil-surround
  ;; need to setup for pending states
  :demand t
  :config
  (global-evil-surround-mode 1)

  ;; when surrounding FOO,
  ;; original: "(" outputs "( FOO )", ")" outputs "(FOO)"
  ;; I want:   "(" outputs "(FOO)",   ")" outputs "( FOO )"
  (let ((left-right-alist '((?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\}))))
    (mapcar (lambda (pair)
              (let* ((left (car pair))
                     (right (cdr pair))
                     (left-cons (assq left evil-surround-pairs-alist))
                     (right-cons (assq right evil-surround-pairs-alist))
                     (left-value (cdr left-cons))
                     (right-value (cdr right-cons)))
                (setcdr left-cons right-value)
                (setcdr right-cons left-value)))
            left-right-alist))

  (general-define-key
   :states '(visual)
   "s" 'evil-surround-region)
  (evil-define-key 'visual evil-snipe-local-mode-map (kbd "s") #'evil-surround-region))


(use-package evil-matchit
  :commands (evilmi-inner-text-object evilmi-outer-text-object evilmi-jump-items)
  :init
  (general-define-key
   :states '(normal visual motion operator)
   "M" 'evilmi-jump-items)
  (general-define-key
   :keymaps '(evil-inner-text-objects-map)
   "m" 'evilmi-inner-text-object)
  (general-define-key
   :keymaps '(evil-outer-text-objects-map)
   "m" 'evilmi-outer-text-object)

  :config
  (global-evil-matchit-mode 1))


(use-package evil-snipe
  :hook ((prog-mode . evil-snipe-local-mode)
         (prog-mode . evil-snipe-override-local-mode))
  :config
  (setq evil-snipe-repeat-keys nil
        evil-snipe-enable-highlight nil)

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
  (define-key evil-normal-state-map (kbd "DEL") 'evil-snipe-repeat-reverse))


(use-package evil-multiedit
  :demand t
  ;; :commands (evil-multiedit-match-all evil-multiedit-match-symbol-and-next evil-multiedit-match-symbol-and-prev)
  :init
  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  (general-define-key
   :states '(normal visual)
   "R" 'evil-multiedit-match-all
   "C-n" 'evil-multiedit-match-symbol-and-next
   "C-p" 'evil-multiedit-match-symbol-and-prev)
  :config
  (define-key evil-multiedit-state-map (kbd "<return>") 'evil-multiedit-toggle-or-restrict-region)
  (define-key evil-multiedit-state-map (kbd "<S-tab>") 'evil-multiedit-prev)
  ;; these have to be loaded before company so they don't shadow tab binding in `company-active-map'
  (define-key evil-multiedit-state-map (kbd "<tab>") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "<tab>") 'tab-indent-or-complete)
  )


(use-package evil-ediff
  :init
  (evil-ediff-init))


(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-define-key
   :states '(normal)
   "+" 'evil-numbers/inc-at-pt
   "-" 'evil-numbers/dec-at-pt))


(push (expand-file-name "targets" jester-submodules-dir) load-path)
(require 'targets)
;; not needed if `targets-setup'?
;; (general-define-key
;;  :keymaps 'evil-visual-state-map
;;  "I" nil
;;  "A" nil)

;; setup has to come before custom bindings
(targets-setup t
 :next-key "N"
 :last-key "L"
 :remote-key "M")

(targets-define-composite-to pair-delimiter
  (("(" ")" pair)
   ("[" "]" pair)
   ("{" "}" pair)
   ("<" ">" pair))
  :bind t
  :next-key "N"
  :last-key "L"
  :remote-key "M"
  :keys "o")

(targets-define-composite-to quote
  (("\"" "\"" quote)
   ("'" "'" quote)
   ("`" "`" quote))
  :bind t
  :next-key "N"
  :last-key "L"
  :remote-key "M"
  :keys "'")

;;----------------------------------------------------------------------------
;; Set initial states for modes.
;;----------------------------------------------------------------------------
(evil-set-initial-state 'debugger-mode 'motion)
;; special mode is for viewing info, e.g. "q" is bound to quit,
;; but it's normal state there so we lose the bindings. Use motion state.
(evil-set-initial-state 'special-mode 'motion)

;; TODO not work
(add-hook! 'snippet-mode-hook (when (string-equal (buffer-name) "+new-snippet+")
                                (evil-insert-state)))

;; when first line is empty, we probably wanna start typing right away.
(add-hook! 'with-editor-mode-hook (when (looking-at "$") (evil-insert-state)))

;;----------------------------------------------------------------------------
;; With-leader keys...
;;----------------------------------------------------------------------------
(jester/with-leader
 "u" 'universal-argument
 "f i" (lambda! (find-file (expand-file-name "init.el" user-emacs-directory)))
 "j f" 'find-function
 "j v" 'find-variable
 "n f" 'narrow-to-defun
 "n p" 'narrow-to-page
 "n v" 'narrow-to-region
 "n r" 'narrow-to-region
 "n w" 'widen
 "q q" 'save-buffers-kill-terminal
 "i r" 'ivy-resume
 "x o" 'just-one-space
 "," 'evil-indent
 "!" 'shell-command)

;;----------------------------------------------------------------------------
;; With-major-leader keys...
;;----------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#simulating-keypresses
(eval `(jester/major-leader-def ,jester-mode-leader (general-key "C-c C-c")))
(eval `(jester/major-leader-def ,jester-mode-leader-emacs (general-key "C-c C-c")))

;;----------------------------------------------------------------------------
;; Other evil keys...
;;----------------------------------------------------------------------------
(general-define-key
 :states '(normal)
 "Q" "@q"
 "gJ" 'jester/evil-join-no-whitespace)

(general-define-key
 :states '(normal)
 :keymaps '(text-mode-map
            conf-mode-map
            prog-mode-map

            messages-buffer-mode-map

            comint-mode-map
            inferior-emacs-lisp-mode-map

            org-mode-map
            markdown-mode-map

            gitconfig-mode-map
            diff-mode-map)
 "<return>" 'switch-to-buffer)

(general-define-key
 :states '(visual)
 "*" 'jester/evil-visual-search-forward
 "#" 'jester/evil-visual-search-backward
 ;; TODO not working
 "<" (lambda! (call-interactively 'evil-shift-left) (evil-visual-restore))
 ">" (lambda! (call-interactively 'evil-shift-right) (evil-visual-restore))
 ;; run macro in the q register on all selected lines
 "Q" ":norm @q RET"

 ;; fix keys bound by motion state
 "d" 'evil-delete)

(general-define-key
 :states 'motion
 "u" 'evil-scroll-up
 "d" 'evil-scroll-down)

(general-define-key
 :states 'motion
 :keymaps '(help-mode-map Man-mode-map)
 "K" 'help-go-back
 "J" 'help-go-forward)

(general-define-key
 :states '(normal motion)
 "*" 'jester/evil-normal-search-forward
 "#" 'jester/evil-normal-search-backward
 "C-q" (lambda! (evil-ex-nohighlight)
                (when (fboundp 'symbol-overlay-remove-all) (symbol-overlay-remove-all))))

(general-define-key
 :states '(normal motion visual)
 "C-u" 'evil-scroll-up
 "C-j" 'evil-scroll-line-down
 "C-k" 'evil-scroll-line-up
 "'" 'evil-goto-mark
 "C-h C-f" 'describe-face
 "C-h p" 'describe-package
 "C-h c" 'describe-char)

(general-define-key
 :states '(normal visual motion operator)
 "[" 'backward-paragraph
 "]" 'forward-paragraph
 "C-a" 'evil-first-non-blank
 "C-e" 'evil-end-of-line)

(general-define-key
 :states '(insert emacs)
 "C-b" 'delete-char
 "C-o" 'evil-open-below
 "C-S-o" 'evil-open-above
 "C-d" 'backward-char
 "C-n" 'next-line
 "C-p" 'previous-line
 "C-a" 'beginning-of-line-text
 "C-e" 'move-end-of-line
 "C-k" 'kill-line
 "C-w" 'evil-delete-backward-word
 "C-v" 'yank
 "M-d" 'backward-word
 "M-b" 'kill-word
 "M-v" 'evil-scroll-down
 "M-c" 'evil-scroll-up
 "H-x" 'kill-region)

;;----------------------------------------------------------------------------
;; Not-so-evil keys...
;;----------------------------------------------------------------------------
(general-define-key
 "C-h p" 'describe-package)

;;----------------------------------------------------------------------------
;; Some functions.
;;----------------------------------------------------------------------------
(evil-define-command jester/evil-normal-search-forward (&optional count)
  "Search symbol under point forward."
  :repeat nil
  (interactive "p")
  (evil-search-word-forward (or count 1) t))

(evil-define-command jester/evil-normal-search-backward (&optional count)
  "Search symbol under point backward."
  :repeat nil
  (interactive "p")
  (evil-search-word-backward (or count 1) t))

(evil-define-command jester/evil-visual-search-forward ()
  "Search the region forward."
  :repeat nil
  (interactive)
  (jester/evil-visual-search t))

(evil-define-command jester/evil-visual-search-backward ()
  "Search the region backward."
  :repeat nil
  (interactive)
  ;; move point to the start of region to jump past the current occurence
  (when (eql (point) (region-end))
    (exchange-point-and-mark))
  (jester/evil-visual-search nil))

(defun jester/evil-visual-search (forward)
  "Search the region."
  (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
    (deactivate-mark)
    (evil-push-search-history string forward)
    ;; pushed to `regexp-search-ring'
    (isearch-update-ring string evil-regexp-search)
    (setq isearch-forward forward)
    (evil-search string forward)))


(provide 'init-evil)
