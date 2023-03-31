(use-package youdao-dictionary
  :init
  ;; Enable Cache
  (setq url-automatic-caching t)
  ;; Set file path for saving search history
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
  ;; Enable Chinese word segmentation support (支持中文分词)
  ;; (setq youdao-dictionary-use-chinese-word-segmentation t)
  (jester/with-leader "y y" 'youdao-dictionary-search-at-point+)
  (jester/with-leader "y i" 'youdao-dictionary-search-from-input)
  :bind ("C-c y" . youdao-dictionary-search-at-point+)
  :hook (youdao-dictionary-mode . evil-motion-state))


(push (expand-file-name "popweb" jester-submodules-dir) load-path)
(push (expand-file-name "popweb/extension/dict" jester-submodules-dir) load-path)
(use-package popweb-dict
  :ensure nil
  :commands popweb-dict-bing-pointer)


(use-package pyim
  :demand t
  :config
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  ;; pyim-probe-isearch-mode
                  ;; pyim-probe-org-structure-template
                  )

                pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  (setq default-input-method "pyim"
        pyim-default-scheme 'wubi
        pyim-page-tooltip (if window-system 'posframe 'popup)
        pyim-page-length 5)
  :bind
  (("M-l" . pyim-convert-string-at-point)
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   ))


(use-package pyim-wbdict
  :demand t
  :after pyim
  :config
  (pyim-wbdict-v98-enable))

(provide 'init-chinese)
