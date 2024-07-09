(use-package gptel
  :config
  (setq gptel-model "moonshot-v1-8k"
        gptel-default-mode 'org-mode
        gptel-backend (gptel-make-openai "Moonshot"
                        :key 'gptel-api-key
                        :host "api.moonshot.cn"
                        :stream t
                        :models '("moonshot-v1-8k"
                                  "moonshot-v1-32k"
                                  "moonshot-v1-128k")))
  (jester/with-major-leader 'gptel-mode-map
    ;; "e" (lambda! (call-interactively 'evil-append-line) (gptel-send) (evil-normal-state))
    "e" (lambda! (save-excursion (let ((evil-move-beyond-eol t))
                                   (evil-end-of-line) (gptel-send))))
    "i" 'jester/ai-insert-new-conversation-instruction))


(defun jester/ai-insert-new-conversation-instruction ()
  "Insert some text to tell ai to start a new conversation, with hints on how he/she/it should express itself."
  (interactive)
  (call-interactively 'evil-append-line)
  (insert "我们开始一个新的对话吧。请记住使用中文来和我对话。但是，对于专业术语，直接使用英文，并且不要加上中文翻译。"))

(provide 'init-ai)
