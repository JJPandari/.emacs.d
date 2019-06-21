(defun jester/ui5-gen-define-callback-params ()
  "According to the declared classes that are imported, generate param symbols accordingly, write the callback function params."
  (interactive)
  (let ((text (buffer-substring-no-properties (search-backward "[") (- (search-forward "]") 1)))
        (fun-start-paren (search-forward "function ("))
        (fun-end-paren (search-forward ")")))
    (backward-char)
    (kill-region fun-start-paren (point))
    (insert (mapconcat 'identity
                       (jester/regexp-matches-in-string "\\/\\([^/',]+\\)',?$" text 1)
                       ", "))))

(jester/with-major-leader 'js2-mode-map
                          "u u" 'jester/ui5-gen-define-callback-params)


(defun jester/build-project ()
  "build the project"
  (interactive)
  ;; view this shell-mode buffer in motion state, then restore default state for shell-mode to insert
  (evil-set-initial-state 'shell-mode 'motion)
  (async-shell-command "~/build-project.sh" "*build-project*")
  (evil-set-initial-state 'shell-mode 'insert))

(jester/with-leader "c u" 'jester/build-project)

(add-hook! 'shell-mode-hook (when (string-equal (buffer-name) "*build-project*")
                              (define-key evil-motion-state-local-map "q" 'bury-buffer)))

(provide 'init-ui5)
