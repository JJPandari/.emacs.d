(push (expand-file-name "emacs-application-framework" jester-submodules-dir) load-path)
(use-package eaf
  :init
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  (use-package epc :defer t)
  :ensure nil
  :demand t
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (setq eaf-browser-dark-mode "false")
  ;; (setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)

  (require 'eaf-evil)

  (define-key key-translation-map (kbd "SPC")
    (lambda (prompt)
      (if (derived-mode-p 'eaf-mode)
          (pcase eaf--buffer-app-name
            ("browser" (if  (string= (eaf-call-sync "call_function" eaf--buffer-id "is_focus") "True")
                           (kbd "SPC")
                         (kbd eaf-evil-leader-key)))
            ("pdf-viewer" (kbd eaf-evil-leader-key))
            ("image-viewer" (kbd eaf-evil-leader-key))
            (_  (kbd "SPC")))
        (kbd "SPC")))))

(provide 'init-eaf)
