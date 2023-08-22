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
  (defconst jester-leader-emacs "S-SPC" "Leader key in emacs state.")
  (defconst jester-mode-leader "," "Mode specific leader key in normal state.")
  (defconst jester-mode-leader-emacs "M-m" "Major-leader key in emacs state.")

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
    (declare (indent 1))
    `(progn (jester/major-leader-def :keymaps ,keymaps ,@args)
            (jester/major-leader-emacs-def :keymaps ,keymaps ,@args)))
  (defmacro jester/with-minor-leader (mode &rest args)
    "Define a leader key sequence using major leader key, but for a minor mode."
    (declare (indent 1))
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
  (evil-move-beyond-eol nil "stay at eol but no further")
  (evil-want-keybinding nil) ; required by evil-collection
  :init
  :config
  (evil-mode 1)
  ;; move by symbol rather than word
  ;; http://emacs.stackexchange.com/a/20717/12854
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-shift-width 2)
  (setq
   evil-normal-state-tag   (propertize "N" 'face '((:background "DarkGoldenrod2" :foreground "black")))
   evil-motion-state-tag   (propertize "M" 'face '((:background "plum3") :foreground "white"))
   evil-visual-state-tag   (propertize "V" 'face '((:background "gray" :foreground "black")))
   evil-insert-state-tag   (propertize "I" 'face '((:background "chartreuse3") :foreground "white"))
   evil-emacs-state-tag    (propertize "E" 'face '((:background "SkyBlue2" :foreground "black")))
   evil-operator-state-tag (propertize "O" 'face '((:background "#ffec8b" :foreground "#93a1a1")))
   evil-replace-state-tag (propertize "R" 'face '((:background "chocolate" :foreground "white")))
   evil-normal-state-cursor '("DarkGoldenrod2" box)
   evil-motion-state-cursor '("plum3" box)
   evil-visual-state-cursor '("gray" (hbar . 2))
   evil-insert-state-cursor '("chartreuse3" (bar . 2))
   evil-emacs-state-cursor '("RoyalBlue" bar)
   evil-operator-state-cursor '("chocolate" box)
   evil-replace-state-cursor '("chocolate" (hbar . 2))
   evil-multiedit-state-cursor '("MediumPurple1" box)
   evil-multiedit-insert-state-cursor '("MediumPurple1" (bar . 2)))

  (evil-define-text-object jester/evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))

  (evil-define-text-object jester/evil-inner-line (count &optional beg end type)
    (list (progn (beginning-of-line-text) (point)) (line-end-position)))

  (general-define-key
   :keymaps 'evil-inner-text-objects-map
   "g" 'jester/evil-inner-buffer
   "<return>" 'jester/evil-inner-line)

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
   "M" 'evilmi-inner-text-object)
  (general-define-key
   :keymaps '(evil-outer-text-objects-map)
   "M" 'evilmi-outer-text-object)

  :config
  (global-evil-matchit-mode 1))


(use-package evil-snipe
  :hook ((prog-mode . evil-snipe-local-mode)
         (prog-mode . evil-snipe-override-local-mode))
  :config
  (setq evil-snipe-repeat-keys nil
        evil-snipe-enable-highlight nil)

  ;; remove default bindings
  (general-define-key
   :states '(normal operator)
   :keymaps 'evil-snipe-local-mode-map
   "s" nil
   "S" nil
   "z" nil
   "Z" nil
   "x" nil
   "X" nil)

  (general-define-key
   :states '(normal visual operator)
   :keymaps 'evil-snipe-local-mode-map
   "g s" 'evil-snipe-s
   "g S" 'evil-snipe-S
   "g c" 'evil-snipe-x
   "g C" 'evil-snipe-X)
  (define-key evil-normal-state-map (kbd "DEL") 'evil-snipe-repeat-reverse))


(use-package evil-collection
  :demand t
  :after evil
  :config
  ;; modes I don't want evil-collection to touch
  (dolist (mode '(company compile comint help info ivy vterm (term term ansi-term multi-term)))
    (setq evil-collection-mode-list (delete mode evil-collection-mode-list)))
  (setq evil-collection-want-unimpaired-p nil
        evil-collection-want-find-usages-bindings nil)
  (evil-collection-init)

  ;; modify a bit after evil-collection setup

  ;; evil-collection sets up keys in `after-load-functions', so setting keys directly won't work
  ;; not sure why but after-load fixes this
  (after-load 'magit
    (general-define-key
     :states '(normal)
     :keymaps '(magit-mode-map magit-diff-mode-map magit-log-mode-map)
     "/" 'jester/swiper-dwim)))


(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
  :init
  (general-define-key
   :states '(normal)
   "+" 'evil-numbers/inc-at-pt
   "-" 'evil-numbers/dec-at-pt))


(use-package evil-goggles
  :after evil
  :demand t
  :custom (evil-goggles-duration 0.1)
  :init
  ;; TODO use append
  ;; (lispyville->                    :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
  ;; (lispyville-<                    :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
  (setq evil-goggles--commands
        '((evil-delete                     :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
          (evil-delete-line                :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
          (evil-org-delete                 :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
          (evil-yank                       :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
          (evil-yank-line                  :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
          (evil-change                     :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (evil-change-line                :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (evil-change-whole-line          :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (evil-indent                     :face evil-goggles-indent-face                :switch evil-goggles-enable-indent                :advice evil-goggles--generic-async-advice)
          (evil-join                       :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
          (evil-join-whitespace            :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
          (evil-fill-and-move              :face evil-goggles-fill-and-move-face         :switch evil-goggles-enable-fill-and-move         :advice evil-goggles--generic-async-advice)
          (evil-shift-left                 :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
          (evil-shift-right                :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
          (evil-org-<                      :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
          (evil-org->                      :face evil-goggles-shift-face                 :switch evil-goggles-enable-shift                 :advice evil-goggles--generic-async-advice)
          (evil-surround-region            :face evil-goggles-surround-face              :switch evil-goggles-enable-surround              :advice evil-goggles--generic-async-advice)
          (evil-commentary                 :face evil-goggles-commentary-face            :switch evil-goggles-enable-commentary            :advice evil-goggles--generic-async-advice)
          (evilnc-comment-operator         :face evil-goggles-nerd-commenter-face        :switch evil-goggles-enable-nerd-commenter        :advice evil-goggles--generic-async-advice)
          (evil-replace-with-register      :face evil-goggles-replace-with-register-face :switch evil-goggles-enable-replace-with-register :advice evil-goggles--generic-async-advice-1)
          (evil-set-marker                 :face evil-goggles-set-marker-face            :switch evil-goggles-enable-set-marker            :advice evil-goggles--set-marker-advice)
          (evil-record-macro               :face evil-goggles-record-macro-face          :switch evil-goggles-enable-record-macro          :advice evil-goggles--record-macro-advice)
          (evil-paste-before               :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)
          (evil-paste-after                :face evil-goggles-paste-face                 :switch evil-goggles-enable-paste                 :advice evil-goggles--paste-advice :after t)
          (lispyville-yank                 :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
          (lispyville-delete               :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
          (lispyville-change               :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (lispyville-yank-line            :face evil-goggles-yank-face                  :switch evil-goggles-enable-yank                  :advice evil-goggles--generic-async-advice)
          (lispyville-delete-line          :face evil-goggles-delete-face                :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
          (lispyville-change-line          :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (lispyville-change-whole-line    :face evil-goggles-change-face                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
          (lispyville-join                 :face evil-goggles-join-face                  :switch evil-goggles-enable-join                  :advice evil-goggles--join-advice)
          (lispyville-comment-or-uncomment :face evil-goggles-nerd-commenter-face        :switch evil-goggles-enable-nerd-commenter        :advice evil-goggles--generic-async-advice)
          (lispyville-prettify             :face evil-goggles-indent-face                :switch evil-goggles-enable-indent                :advice evil-goggles--generic-async-advice)))
  :hook (after-init . evil-goggles-mode)
  :config
  ;; (evil-goggles-use-diff-faces)
  (evil-goggles-use-diff-refine-faces))


(use-package evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "fd"
        evil-escape-unordered-key-sequence t)
  ;; (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :demand t
  :config
  (evil-escape-mode))


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
               :remote-key "r")

;; define jester/line-text for thingatpt/evil/targets
(put 'jester/line-text 'forward-op #'jester/line-text-forward)

(defun jester/line-text-forward ()
  "Forward 1 line-text text object."
  ;; TODO line is planned by targets
  )

(targets-define-to line-text 'jester/line-text nil object
                   :bind t
                   :keys "q" ;; TODO how to use <return> here
                   :next-key "N"
                   :last-key "L"
                   :remote-key "r")

;; TODO expand-region is more reliable, investigate
(targets-define-composite-to pair-delimiter
  (("(" ")" pair)
   ("[" "]" pair)
   ("{" "}" pair)
   ("<" ">" pair))
  :bind t
  :next-key "N"
  :last-key "L"
  :remote-key "r"
  :keys "o")

(targets-define-composite-to quote
  (("\"" "\"" quote)
   ("'" "'" quote)
   ("`" "`" quote))
  :bind t
  :next-key "N"
  :last-key "L"
  :remote-key "r"
  :keys "'")

;;----------------------------------------------------------------------------
;; Set initial states for modes.
;;----------------------------------------------------------------------------
(evil-set-initial-state 'debugger-mode 'motion)
(evil-set-initial-state 'messages-buffer-mode 'motion)

(advice-add
 'yas-new-snippet :after
 (lambda (_)
   "When creating a new snippet, start with insert state."
   (evil-insert-state))
 '((name . "insert-state")))

;; when first line is empty, we probably wanna start typing right away.
(add-hook! 'with-editor-mode-hook (when (eolp) (evil-insert-state)))

;;----------------------------------------------------------------------------
;; With-leader keys...
;;----------------------------------------------------------------------------
(jester/with-leader
 "f i" (lambda! (find-file (expand-file-name "init.el" user-emacs-directory)))
 "f o" (lambda! (find-file "~/org/todo.org"))
 "f a" 'find-alternate-file
 "h f" 'describe-function
 "h F" 'list-faces-display
 "h v" 'describe-variable
 "h s" 'describe-symbol
 "h c" 'describe-char
 "h k" 'describe-key
 "h p" 'describe-package
 "h m" 'describe-mode
 "n f" 'narrow-to-defun
 "n p" 'narrow-to-page
 "n v" 'narrow-to-region
 "n r" 'narrow-to-region
 "n w" 'widen
 "p l" 'list-packages
 "q q" 'save-buffers-kill-terminal
 "t o" 'just-one-space
 "t a" 'aggressive-indent-mode
 ;; NOTE: "a" "'" are still there for the taking
 "," 'evil-indent
 "." 'jester/evil-portal-set-portal-location
 ">" 'jester/evil-portal-clear-portal-location
 "!" 'shell-command
 "<" 'evil-shift-left
 ">" 'evil-shift-right)

(general-define-key
 :keymaps 'universal-argument-map
 (concat jester-leader " u") 'universal-argument-more)

;;----------------------------------------------------------------------------
;; With-major-leader keys...
;;----------------------------------------------------------------------------
;; https://github.com/noctuid/general.el#simulating-keypresses
(eval `(jester/major-leader-def ,jester-mode-leader (general-key "C-c C-c")))
(eval `(jester/major-leader-def ,jester-mode-leader-emacs (general-key "C-c C-c")))

;;----------------------------------------------------------------------------
;; other evil keys / keys for specific evil states
;;----------------------------------------------------------------------------
(general-define-key
 :states '(normal)
 "r" 'evil-redo
 "t" 'evil-replace
 "Q" "@q"
 "gJ" 'jester/evil-join-no-whitespace
 "z i" 'evil-emacs-state
 "<C-i>" 'beginning-of-line-text
 "C-o" 'move-end-of-line
 "<" nil
 ">" nil)

(general-define-key
 :states '(normal)
 :keymaps '(text-mode-map
            conf-mode-map
            prog-mode-map
            restclient-mode-map

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
 ;; run macro in the q register on all selected lines
 "Q" ":norm @q <return>"

 ;; fix keys bound by motion state
 "r" 'evil-replace
 "d" 'evil-delete)

(general-define-key
 :states 'motion
 "u" 'evil-scroll-up
 "d" 'evil-scroll-down
 "<escape>" 'evil-motion-state)

(general-define-key
 :states '(normal motion)
 "*" 'jester/evil-normal-search-forward
 "#" 'jester/evil-normal-search-backward
 "C-q" (lambda! (evil-ex-nohighlight)
                (when (fboundp 'symbol-overlay-remove-all) (call-interactively 'symbol-overlay-remove-all))))

(general-define-key
 :states '(normal motion visual)
 "'" 'evil-goto-mark)

(general-define-key
 :states '(normal visual motion operator)
 "C-," 'evil-first-non-blank
 "C-." 'evil-end-of-line)

(general-define-key
 :states '(insert)
 "M-v" 'evil-scroll-down
 "M-c" 'evil-scroll-up
 "S-<backspace>" 'delete-forward-char
 "C-," 'backward-char
 "C-." 'forward-char
 "C-n" 'next-line
 "C-p" 'previous-line
 "<C-i>" 'beginning-of-line-text
 "C-o" 'move-end-of-line
 "C-k" 'kill-line
 "H-w" 'evil-delete-backward-word
 "H-x" 'kill-region
 "H-c" 'kill-ring-save
 "H-v" 'yank
 ;; reserve C-r for commands similar to backward-search
 "C-y" 'evil-paste-from-register
 "M-." 'forward-word
 "M-," 'backward-word
 "M-<backspace>" 'kill-word)

(general-define-key
 :states '(emacs)
 "M-v" 'evil-scroll-down
 "M-c" 'evil-scroll-up
 "S-<backspace>" 'delete-forward-char
 "C-," 'backward-char
 "C-." 'forward-char
 "C-n" 'next-line
 "C-p" 'previous-line
 "<C-i>" 'beginning-of-line-text
 "C-o" 'move-end-of-line
 "C-k" 'kill-line
 "H-w" 'backward-kill-word
 "H-x" 'kill-region
 "H-c" 'kill-ring-save
 "H-v" 'yank
 ;; reserve C-r for commands similar to backward-search
 ;; "C-y" 'evil-paste-from-register
 "M-." 'forward-word
 "M-," 'backward-word
 "M-<backspace>" 'kill-word
 "<escape>" 'evil-exit-emacs-state)

;;----------------------------------------------------------------------------
;; Not-so-evil keys...
;;----------------------------------------------------------------------------
(general-define-key
 "C-h C-f" 'list-faces-display
 "C-h p" 'describe-package
 "C-h c" 'describe-char
 "C-h K" 'find-function-on-key
 ;; copy insert state bindings here, so keys do the same when evil disabled
 "M-v" 'evil-scroll-down
 "M-c" 'evil-scroll-up
 "S-<backspace>" 'delete-forward-char
 "C-," 'backward-char
 "C-." 'forward-char
 "C-n" 'next-line
 "C-p" 'previous-line
 "<C-i>" 'beginning-of-line-text
 "C-o" 'move-end-of-line
 "C-k" 'kill-line
 "H-w" 'backward-kill-word
 "H-x" 'kill-region
 "H-c" 'kill-ring-save
 "H-v" 'yank
 "H-g" 'keyboard-quit
 ;; reserve C-r / H-r for commands similar to backward-search
 ;; "C-y" 'evil-paste-from-register
 "M-." 'forward-word
 "M-," 'backward-word
 "M-<backspace>" 'kill-word
 "<escape>" 'keyboard-quit)

;;----------------------------------------------------------------------------
;; Some functions.
;;----------------------------------------------------------------------------
(defun jester/evil-paste-after--from-copy-register (&optional count)
  "So often do we paste from the copy register, let's make it a command and bind it to a key and forget about \"0p"
  (interactive "p")
  (evil-paste-after (or count 1) ?0))

(defun jester/evil-paste-before--from-copy-register (&optional count)
  "So often do we paste from the copy register, let's make it a command and bind it to a key and forget about \"0p"
  (interactive "p")
  (evil-paste-before (or count 1) ?0))

(general-define-key
 :states '(normal visual)
 "z p" 'jester/evil-paste-after--from-copy-register
 "z P" 'jester/evil-paste-before--from-copy-register)


(setq isearch-lazy-count t)
;; TODO use symbol-overlay ... or?
;; (defun jester/evil-search-region-or-symbol-forward (count forward)
;;   "Search the region."
;;   (isearch-forward nil ))

;; (evil-define-command jester/evil-search-under-point-forward (&optional count)
;;   "Search symbol under point forward."
;;   :repeat nil
;;   (interactive "p")
;;   (jester/evil-search-region-or-symbol-forward (or count 1) t))



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
  (let ((regex (regexp-quote (buffer-substring-no-properties (region-beginning) (region-end)))))
    (deactivate-mark)
    (evil-push-search-history regex forward)
    ;; pushed to `regexp-search-ring'
    (isearch-update-ring regex evil-regexp-search)
    (setq isearch-forward forward)
    (evil-search regex forward evil-regexp-search)))

;;----------------------------------------------------------------------------
;; "portal"
;;----------------------------------------------------------------------------
(defvar-local jester-evil-portal-location 0 "Position marked to jump back to after a evil delete or yank.")

(defun jester/evil-portal-set-portal-location ()
  "Mark the current point for jumping back after next evil yank or delete."
  (interactive)
  (setq jester-evil-portal-location (point))
  (evil-goggles--show-async-hint (line-beginning-position) (1+ (line-end-position))))

(defun jester/evil-portal-clear-portal-location ()
  "Clear jump back location. See `jester/evil-portal-set-portal-location'."
  (interactive)
  (setq jester-evil-portal-location 0))

(defun jester/evil-portal-jump-advice (&rest _)
  "Advice to jump back to marked position (if any) after an evil yank or delete."
  (when (called-interactively-p 'interactive)
    (when (/= jester-evil-portal-location 0)
      (goto-char jester-evil-portal-location)
      (setq jester-evil-portal-location 0))))
(dolist (cmd '(evil-yank evil-delete lispyville-yank lispyville-delete))
  (advice-add cmd :after 'jester/evil-portal-jump-advice))

(evil-define-motion jester/cycle-line-beginning-end (count)
  "Go to line text beginning, line end, line very beginning, in turn."
  (cl-block 'my-return
    (when (and (looking-at "[^\s]") (looking-back "^\s*")) (evil-end-of-line) (cl-return-from 'my-return)) ; at beg of line text
    (when (looking-at (if evil-move-beyond-eol "$" ".$")) (evil-beginning-of-line) (cl-return-from 'my-return)) ; at end of line
    (when (bolp) (evil-first-non-blank) (cl-return-from 'my-return)) ; at very beg of line
    (evil-first-non-blank)))
(general-define-key
 :states '(normal visual operator motion)
 "0" 'jester/cycle-line-beginning-end)

;; TODO try citre-read or clue

;; TODO show bookmark name in fringe


(provide 'init-evil)
