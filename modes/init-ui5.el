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

(provide 'init-ui5)
