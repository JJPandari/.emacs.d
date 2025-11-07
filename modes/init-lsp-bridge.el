;; -*- lexical-binding: t -*-
(use-package lsp-bridge
  :ensure nil
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                        :files ("*" (:exclude ".git" "test"))
                        :build (:not compile))
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

  (jester/with-leader
   "s r" 'lsp-bridge-rename)

  (jester/with-minor-leader 'lsp-bridge-mode
    "a" 'lsp-bridge-code-action
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

;;----------------------------------------------------------------------------
;; commands for tab key, do complete or yas
;;----------------------------------------------------------------------------
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

;; pop lsp doc on hover
(defun jester/pop-lsp-doc-post-command ()
  "pop lsp doc on hover"
  (when (and (lsp-bridge-has-lsp-server-p)
             (not (member (format "%s" this-command)
                          '("self-insert-command" "org-self-insert-command"
                            "lsp-bridge-diagnostic-jump-next" "lsp-bridge-diagnostic-jump-prev"))))
    (run-with-idle-timer 3 nil 'lsp-bridge-popup-documentation)))

;; (add-hook 'post-command-hook 'jester/pop-lsp-doc-post-command)
;; (remove-hook 'post-command-hook 'jester/pop-lsp-doc-post-command)



(use-package orderless
  :autoload (orderless-initialism orderless-flex))


(provide 'init-lsp-bridge)
