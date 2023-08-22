;; -*- lexical-binding: t -*-
(use-package lsp-bridge
  :ensure nil
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                        :files ("*" (:exclude ".git" "test")))
  :init
  ;; TODO a new face to inherit markdown mode
  (setq acm-enable-tabnine nil
        ;; TODO acm-enable-codeium
        acm-enable-yas nil
        ;; TODO acm-enable-citre
        lsp-bridge-enable-completion-in-string t
        acm-candidate-match-function 'orderless-flex
        acm-enable-quick-access t ; select candidate with number key
        acm-quick-access-use-number-select t
        lsp-bridge-enable-signature-help nil
        lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame
        lsp-bridge-signature-show-with-frame-position "top-left"
        lsp-bridge-enable-hover-diagnostic t)
  :demand t
  :config
  (global-lsp-bridge-mode)

  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)

  ;; TODO what's the face for error pop?
  (set-face-attribute 'lsp-bridge-ref-font-lock-diagnostic nil :foreground (face-attribute 'error :foreground))

  (jester/with-minor-leader 'lsp-bridge-mode
    "d" 'lsp-bridge-popup-documentation)

  (general-define-key
   :states '(insert emacs)
   "<tab>" 'jester/start-complete-or-yas)
  (general-define-key
   :states '(insert emacs)
   :definer 'minor-mode
   :keymaps 'acm-mode
   "TAB" nil
   "<tab>" 'jester/do-complete-or-yas
   "<C-m>" 'acm-insert-common
   "RET" 'newline
   "<return>" 'newline)
  (dotimes (i 10)
    (general-define-key
     :keymaps 'acm-mode-map
     (format "M-%d" i) (eval `(lambda! (insert ,(number-to-string i)))))))

;; https://emacs.stackexchange.com/a/7925/12854
(defun jester/do-complete-or-yas ()
  "`yas-expand' or complete the symbol."
  (interactive)
  (when (or (not yas-minor-mode)
            (null (jester/yas-expand-no-prompt)))
    (call-interactively 'acm-complete)))

(defun jester/start-complete-or-yas ()
  (interactive)
  (when (or (not yas-minor-mode)
            (null (jester/yas-expand-no-prompt)))
    (lsp-bridge-popup-complete-menu)))



(use-package orderless
  :autoload (orderless-initialism orderless-flex))


(provide 'init-lsp-bridge)
