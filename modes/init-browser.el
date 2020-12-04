(defvar jester-default-search-engine "https://duckduckgo.com/?q=%s"
  "URL used for searching in browser, \"%s\" is the placeholder for query.")
(defun jester/make-site-search-url (site-domain)
  "Return a url to search `SITE-DOMAIN' with `jester-default-search-engine'."
  (s-replace "%s" (concat "%s+site:" site-domain) jester-default-search-engine))
(defvar jester-web-searcher-list
  `(("d" "DuckDuckGo" "https://duckduckgo.com/?q=%s")
    ("b" "Bing" "https://www.bing.com/search?q=%s")
    ("g" "Google" "https://www.google.com/search?q=%s")
    ("so" "Stackoverflow" ,(jester/make-site-search-url "stackoverflow.com"))
    ("es" "Emacs StackExchange" ,(jester/make-site-search-url "emacs.stackexchange.com"))
    ("mdn" "MDN" ,(jester/make-site-search-url "developer.mozilla.org"))
    ("rs" "rust" ,(jester/make-site-search-url "rust-lang.org"))
    ("ng" "nginx" ,(jester/make-site-search-url "nginx.com"))
    ("ws" "wireshark" ,(jester/make-site-search-url "www.wireshark.org")))
  "List for sites I want to search.
Each of the list is a list like (ABBREV SITE-NAME URL), where ABBREV will be used to *lead* the user input.")

(defun jester/search-web ()
  "Search a site by typing SHORTHAND<SPC>QUERY. ABBREV is found in `jester-web-searcher-list'. If not starting with any shorthand, use `jester-default-search-engine'."
  (interactive)
  (ivy-read "search the web: "
            (mapcar (lambda (l) (concat (car l) " " (cadr l))) jester-web-searcher-list)
            :require-match nil
            :action (lambda (input)
                      (-let (((abbrev user-query) (s-split " " input)))
                        (browse-url (if-let ((rest (alist-get abbrev jester-web-searcher-list nil nil 'string=)))
                                        (s-replace "%s" user-query (cadr rest))
                                      (s-replace "%s" input jester-default-search-engine)))))))

(jester/with-leader "s o" 'jester/search-web)


(provide 'init-browser)
