;; (push (expand-file-name "apheleia" jester-submodules-dir) load-path)
(use-package apheleia
  :init
  (jester/with-major-leader '(js2-mode-map web-mode-map typescript-mode-map)
    "f" 'apheleia-format-buffer)
  :commands (apheleia-format-buffer))


(provide 'init-formatter)
