;; -*- lexical-binding: t -*-
(use-package lsp-bridge
  :ensure nil
  :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                        :files ("*" (:exclude ".git" "test")))
  :init
  (setq acm-enable-tabnine nil
        ;; TODO acm-enable-codeium
        acm-enable-yas nil
        ;; TODO acm-enable-citre
        acm-candidate-match-function 'orderless-flex
        acm-enable-quick-access t ; only decides whether indexes are shown
        lsp-bridge-signature-show-function 'lsp-bridge-signature-posframe
        lsp-bridge-enable-hover-diagnostic t)
  :demand t
  :config
  (plist-put lsp-bridge-signature-posframe-params :poshandler 'posframe-poshandler-frame-top-center)

  (global-lsp-bridge-mode)

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
   "RET" nil
   "<return>" nil)
  (dotimes (i 10)
    (general-define-key
     :keymaps 'acm-mode-map
     (number-to-string i) 'acm-complete-quick-access)
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

;; TODO flycheck


(provide 'init-lsp-bridge)
