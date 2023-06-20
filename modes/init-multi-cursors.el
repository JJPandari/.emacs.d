(use-package evil-multiedit
  :demand t
  ;; :commands (evil-multiedit-match-all evil-multiedit-match-symbol-and-next evil-multiedit-match-symbol-and-prev)
  :init
  (setq evil-multiedit-use-symbols t)
  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  (general-define-key
   :states '(normal visual)
   "R" 'jester/evil-multiedit-match-all
   "C-n" 'evil-multiedit-match-and-next
   "C-p" 'evil-multiedit-match-and-prev)
  (defun jester/evil-multiedit-match-all ()
    "Match all occurences. with prefix arg, exclude those in comments or strings."
    (interactive)
    (evil-multiedit-match-all)
    (when current-prefix-arg
      (save-excursion
        (let ((pos))
          (iedit-goto-first-occurrence)
          (setq pos (point))
          (unless (jester/in-expression-area-p)
            (evil-multiedit-toggle-or-restrict-region))
          (while (evil-multiedit-next)
            (unless (jester/in-expression-area-p)
              (evil-multiedit-toggle-or-restrict-region)))))))
  :config
  (general-define-key
   :states '(normal)
   :keymaps 'evil-multiedit-mode-map
   "<return>" 'evil-multiedit-toggle-or-restrict-region
   "<tab>" 'evil-multiedit-next
   "<S-tab>" 'evil-multiedit-prev
   "C-n" 'evil-multiedit-match-and-next
   "C-p" 'evil-multiedit-match-and-prev)
  (general-define-key
   :states '(insert emacs)
   :keymaps 'evil-multiedit-mode-map
   "<return>" 'newline
   "<tab>" 'jester/start-complete
   "C-a" 'evil-multiedit-beginning-of-line
   "C-e" 'evil-multiedit-end-of-line)
  )


(use-package evil-mc
  :init
  ;; don't bind any keys
  (setq evil-mc-key-map (make-sparse-keymap))
  ;; (general-define-key
  ;;  :states '(normal visual)
  ;;  "K" 'evil-mc-make-all-cursors
  ;;  "M-n" 'evil-mc-make-and-goto-next-match
  ;;  "M-p" 'evil-mc-make-and-goto-prev-match
  ;;  "M-j" 'evil-mc-make-cursor-move-next-line
  ;;  "M-k" 'evil-mc-make-cursor-move-prev-line
  ;;  "<M-return>" 'jester/evil-mc-toggle-cursors-pause)
  (defun jester/evil-mc-toggle-cursors-pause ()
    "Toggle between pausing or resuming all cursors."
    (interactive)
    (if evil-mc-frozen
        (evil-mc-resume-cursors)
      (evil-mc-pause-cursors)))
  :commands (evil-mc-make-all-cursors evil-mc-make-cursor-here
                                      evil-mc-make-cursor-move-next-line evil-mc-make-cursor-move-prev-line
                                      evil-mc-make-and-goto-next-match evil-mc-make-and-goto-prev-match)
  :config
  (advice-add 'evil-force-normal-state :after 'evil-mc-undo-all-cursors)
  ;; FIXME activate mode after using some command may be buggy, but it's the only way to lazy-load evil-mc now.
  (global-evil-mc-mode 1)

  ;; Add custom commands to whitelisted commands
  ;; TODO not work
  (dolist (fn '(string-inflection-kebab-case
                string-inflection-camelcase string-inflection-lower-camelcase
                jester/expand-yas-or-complete-company))
    (push (cons fn '((:default . evil-mc-execute-default-call)))
          evil-mc-custom-known-commands)))


;; https://github.com/abo-abo/hydra/wiki/multiple-cursors
(use-package multiple-cursors
  :init
  (general-define-key
   :states '(emacs)
   "M-j" 'mc/mark-next-like-this
   "M-k" 'mc/mark-previous-like-this
   "M-J" 'mc/skip-to-next-like-this
   "M-K" 'mc/skip-to-previous-like-this
   "M-n" 'mc/mark-next-like-this-symbol
   "M-p" 'mc/mark-previous-like-this-symbol
   "M-N" 'mc/skip-to-next-like-this
   "M-P" 'mc/skip-to-previous-like-this)
  (general-define-key
   :states '(normal visual)
   "M-j" 'jester/emacs-state-and-press
   "M-k" 'jester/emacs-state-and-press
   "M-n" 'jester/emacs-state-and-press
   "M-p" 'jester/emacs-state-and-press)
  (general-define-key
   :states '(emacs)
   :definer 'minor-mode
   :keymaps 'multiple-cursors-mode
   "<escape>" 'mc/keyboard-quit)
  (general-define-key :keymaps 'mc/keymap "<return>" nil))

;; TODO kinda works, but not totally
(defun jester/emacs-state-and-press ()
  "Enter emacs state, find the command for the pressed keys in emacs state, call it."
  (interactive)
  (evil-emacs-state)
  (message "%s|%s" (evil-emacs-state-p) (this-command-keys))
  (general-key (this-command-keys)))

(defhydra hydra-multiple-cursors (:hint nil :exit nil)
  "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("|" mc/vertical-align)
  ("s" mc/mark-all-in-region-regexp :exit t)
  ("0" mc/insert-numbers :exit t)
  ("A" mc/insert-letters :exit t)
  ("<mouse-1>" mc/add-cursor-on-click)
  ;; Help with click recognition in this hydra
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore)
  ("q" nil))


(provide 'init-multi-cursors)
