;;----------------------------------------------------------------------------
;; Config general.
;;----------------------------------------------------------------------------
(use-package general
  :demand t
  :config
  (defconst jester/leader "SPC")
  (defconst jester/leader-emacs "M-m")
  (defconst jester/major-leader ",")
  (defconst jester/major-leader-emacs "M-S-m")
  (general-create-definer jester/leader-def :prefix jester/leader :states ('normal 'visual))
  (general-create-definer jester/leader-emacs-def :prefix jester/leader-emacs)
  (general-create-definer jester/major-leader-def :prefix jester/major-leader)
  (general-create-definer jester/major-leader-emacs-def :prefix jester/major-leader-emacs))

;;----------------------------------------------------------------------------
;; Config evil.
;;----------------------------------------------------------------------------
(use-package evil
  :demand t
  :custom
  (evil-want-Y-yank-to-eol t "Y yanks to eol, not whole line")
  :init
  :config
  (evil-mode 1)
  (setq
   evil-normal-state-tag   (propertize "N" 'face '((:background "DarkGoldenrod2" :foreground "black")))
   evil-emacs-state-tag    (propertize "E" 'face '((:background "SkyBlue2" :foreground "black")))
   evil-insert-state-tag   (propertize "I" 'face '((:background "chartreuse3") :foreground "white"))
   evil-motion-state-tag   (propertize "M" 'face '((:background "plum3") :foreground "white"))
   evil-visual-state-tag   (propertize "V" 'face '((:background "gray" :foreground "black")))
   evil-operator-state-tag (propertize "O" 'face '((:background "#ffec8b" :foreground "#93a1a1")))
   evil-evilified-state-tag (propertize "N'" 'face '((:background "#ffb90f" :foreground "black")))))

;;----------------------------------------------------------------------------
;; Set initial states for modes.
;;----------------------------------------------------------------------------
(add-hook 'with-editor-mode-hook (lambda () (evil-insert-state)))

;;----------------------------------------------------------------------------
;; Bind some evil or util functions after general and evil are loaded.
;;----------------------------------------------------------------------------

(general-def (:states visual
                      (kbd "<") "<gv"
                      (kbd ">") ">gv"))

(evil-define-key 'normal
  ;; run the macro in the q register
  "Q" "@q")

(evil-define-key 'visual
  ;; run macro in the q register on all selected lines
  "Q" (kbd ":norm @q RET"))

(global-set-key (kbd "C-h p") 'describe-package)
(global-set-key (kbd "C-;") 'insert-or-remove-trailing-semi)
(global-set-key (kbd "C-,") 'insert-or-remove-trailing-comma)
(define-key evil-normal-state-map (kbd "z k") #'jester/move-line-up)
(define-key evil-normal-state-map (kbd "z j") #'jester/move-line-down)


(provide 'init-evil)
