;; (use-package eglot
;;   :commands (eglot eglot-ensure))


(use-package lsp-mode
  :custom
  (lsp-prefer-flymake nil)
  (lsp-restart 'auto-restart)
  (lsp-enable-symbol-highlighting nil)
  (lsp-prefer-capf nil)
  :init
  :hook ((rust-mode . lsp) (typescript-mode . lsp))
  :commands (lsp lsp-deferred))

;; lsp-ui and company-lsp auto configured by lsp-mode
(use-package lsp-ui
  :custom ((lsp-ui-sideline-enable nil)
           (lsp-ui-flycheck-enable t)
           (lsp-ui-doc-max-height 50)
           (lsp-ui-doc-alignment 'frame-left-or-right-other))
  :init
  ;; (custom-set-faces `(lsp-ui-doc-background
  ;;                     ((t (:background ,(face-attribute 'hl-line :background))))))
  (setq lsp-ui-doc-border (face-attribute 'default :foreground))
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-alignment 'frame-left-or-right-other)
  (defun lsp-ui-doc--move-frame (frame)
    "Place our FRAME on screen."
    (-let* (((left top right _bottom) (window-edges nil nil nil t))
            (window (frame-root-window frame))
            ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
            (width (+ width (* (frame-char-width frame) 1))) ;; margins
            (char-h (frame-char-height))
            (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
            (frame-right (pcase lsp-ui-doc-alignment
                           ('window right)
                           (_ (frame-pixel-width))))
            (frame-resize-pixelwise t)
            ((window-left-edge window-top-edge window-right-edge _) (window-absolute-pixel-edges))
            (window-left-right-center (/ (+ window-left-edge window-right-edge) 2))
            ((frame-left-edge frame-top-edge frame-right-edge _) (frame-edges))
            (frame-left-right-center (/ (+ frame-left-edge frame-right-edge) 2)))
      (set-frame-size frame width height t)
      (if (eq lsp-ui-doc-position 'at-point)
          (lsp-ui-doc--mv-at-point frame height left top)
        (set-frame-position frame
                            (if (and (eq lsp-ui-doc-alignment 'frame-left-or-right-other)
                                     (> window-left-right-center frame-left-right-center))
                                10
                              (max (- frame-right width 10 (frame-char-width)) 10))
                            (pcase lsp-ui-doc-position
                              ('top (pcase lsp-ui-doc-alignment
                                      ('window (+ top 10))
                                      (_ 10)))
                              ('bottom (pcase lsp-ui-doc-alignment
                                         ('window (- (+ (lsp-ui-doc--line-height 'mode-line) (- window-top-edge frame-top-edge))
                                                     height
                                                     10))
                                         (_ (- (frame-pixel-height)
                                               height
                                               10)))))))))
  )

(use-package company-lsp
  :init
  (general-define-key
   :states '(insert emacs)
   :keymaps 'prog-mode-map
   "C-." 'company-lsp)
  :after company
  :commands company-lsp)

;; TODO
;; (set-lookup-handlers! 'lsp-ui-mode :async t
;;                       :definition 'lsp-ui-peek-find-definitions
;;                       :references 'lsp-ui-peek-find-references)

;; (use-package lsp-vue
;;   :demand t)


(provide 'init-lsp)
