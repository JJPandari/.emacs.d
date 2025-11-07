(use-package gptel
  :init
  (jester/with-leader "i" 'gptel)

  (jester/with-minor-leader 'gptel-mode
    ;; "e" (lambda! (call-interactively 'evil-append-line) (gptel-send) (evil-normal-state))
    "e" (lambda! (save-excursion (let ((evil-move-beyond-eol t))
                                   (evil-end-of-line) (gptel-send))))
    "i" 'jester/ai-insert-new-conversation-instruction)
  :config
  (setq gptel-kimi-backend (gptel-make-openai "Moonshot"
                             :key 'gptel-api-key ; default value of this var is `gptel-api-key-from-auth-source'
                             :host "api.moonshot.cn"
                             :stream t
                             :models '("moonshot-v1-8k"
                                       "moonshot-v1-32k"
                                       "moonshot-v1-128k")))
  (setq gptel-deepseek-backend (gptel-make-openai "DeepSeek"
                                 :key 'gptel-api-key ; default value of this var is `gptel-api-key-from-auth-source'
                                 :host "api.deepseek.com"
                                 :stream t
                                 :endpoint "/chat/completions"
                                 :models '("deepseek-chat"
                                           "deepseek-reasoner")))

  ;; (setq gptel-claude-backend (gptel-make-anthropic "Claude"
  ;;                              :key 'gptel-api-key ; default value of this var is `gptel-api-key-from-auth-source'
  ;;                              :host "api.deepseek.com"
  ;;                              :stream t
  ;;                              :endpoint "/chat/completions"
  ;;                              :models '("deepseek-chat"
  ;;                                        "deepseek-reasoner")))

  (setq gptel-default-mode 'markdown-mode
        gptel-backend gptel-deepseek-backend
        gptel-model "deepseek-chat"))

(defun jester/ai-insert-new-conversation-instruction ()
  "Insert some text to tell ai to start a new conversation, with hints on how he/she/it should express itself."
  (interactive)
  (call-interactively 'evil-append-line)
  (insert "我们开始一个新的对话吧。请记住使用中文来和我对话。但是，对于专业术语，直接使用英文，并且不要加上中文翻译。"))


;; (use-package aider
;;   :straight (:host github :repo "tninja/aider.el" :files ("aider.el"))
;;   :config
;;   (setq aider-args '("--model" "deepseek-reasoner"))
;;   ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)

;;   ;; Use claude-3-5-sonnet cause it is best in aider benchmark
;;   ;; (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022"))
;;   ;; (setenv "ANTHROPIC_API_KEY" anthropic-api-key)
;;   ;; Or use chatgpt model since it is most well known
;;   ;; (setq aider-args '("--model" "o3-mini"))
;;   ;; (setenv "OPENAI_API_KEY" <your-openai-api-key>)
;;   ;; Or use gemini v2 model since it is very good and free
;;   ;; (setq aider-args '("--model" "gemini/gemini-exp-1206"))
;;   ;; (setenv "GEMINI_API_KEY" <your-gemini-api-key>)
;;   ;; Or use your personal config file
;;   ;; (setq aider-args `("--config" ,(expand-file-name "~/.aider.conf.yml")))

;;   ;; ;;
;;   ;; Optional: Set a key binding for the transient menu
;;   (jester/with-leader "a" 'aider-transient-menu))

(provide 'init-ai)
