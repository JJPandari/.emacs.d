(setq ui5-jq-ns "jquery.sap.global")

(setq ui5-class-list (list ui5-jq-ns
                           "sap/base/util/ObjectPath"
                           "sap/base/util/UriParameters"
                           "sap/base/Log"
                           "sap/base/security/URLWhitelist"
                           "sap/base/util/deepEqual"
                           "sap/ui/util/Storage"
                           "sap/ui/events/KeyCodes"
                           "sap/ui/core/mvc/Controller"
                           "sap/ui/core/mvc/JSView"
                           "sap/base/security/encodeXML"
                           "sap/ui/Device"
                           "sap/ui/util/Mobile"
                           "sap/base/i18n/ResourceBundle"
                           "sap/base/util/uid"))

(defun ui5-find-dependency-and-param-area ()
  "Find the start and end position of dependency list and callback params for sap.ui.define.
Returns a plist including these keys:
:dep-start
:dep-end
:param-start
:param-end"
  (save-match-data
    (let ((pos-plist))
      (goto-char (point-min))
      (re-search-forward (rx (sequence bol
                                       "sap.ui.define("
                                       (0+ (not (any "\n")))
                                       "["
                                       eol)))
      (setq pos-plist (plist-put pos-plist :dep-start (point)))
      (search-forward "], function (")
      (setq pos-plist (plist-put pos-plist :param-start (point)))
      (setq pos-plist (plist-put pos-plist :dep-end (line-beginning-position)))
      (search-forward ")")
      (backward-char)
      (setq pos-plist (plist-put pos-plist :param-end (point)))
      pos-plist)))

(defun ui5-get-dependency-list ()
  "Returns the current dependency list for sap.ui.define."
  (let* ((area (ui5-find-dependency-and-param-area))
         (trimmed-text (replace-regexp-in-string (rx (0+ (or "\t" " " "\n")))
                                                 ""
                                                 (buffer-substring-no-properties
                                                  (plist-get area :dep-start)
                                                  (plist-get area :dep-end)))))
    (if (string-equal trimmed-text "")
        '()
      (mapcar (lambda (quoted) (substring quoted 1 -1))
              (s-split "," trimmed-text)))))

(defun ui5-write-dependencies-and-params (dependency-list)
  "Re-write the dependency list in sap.ui.define. `DEPENDENCY-LIST' is a list of namespace strings."
  (save-excursion
    (let ((area (ui5-find-dependency-and-param-area)))
      (setq dependency-list
            (sort (mapcar (lambda (namespace)
                            (unless (string-equal namespace ui5-jq-ns)
                              (setq namespace (s-replace "." "/" namespace)))
                            namespace)
                          dependency-list)
                  'string<))
      ;; write param first. If we write dependencies first, param positions would be stale.
      (goto-char (plist-get area :param-start))
      (delete-region (point)
                     (plist-get area :param-end))
      (insert (s-join ", " (mapcar (lambda (namespace)
                                     (if (string-equal namespace ui5-jq-ns)
                                         "jQuery"
                                       (-last-item (s-split "/" namespace))))
                                   dependency-list)))
      (goto-char (plist-get area :dep-start))
      (delete-region (point)
                     (plist-get area :dep-end))
      (insert (concat "\n"
                      (s-join ",\n" (mapcar (lambda (namespace)
                                              (concat "'" namespace "'"))
                                            dependency-list))
                      "\n"))
      (indent-region (plist-get area :dep-start) (plist-get area :dep-end)))))

(defun ui5-organize-dependencies ()
  "Sort dependency list for sap.ui.define, write corresponding callback params to param list."
  (interactive)
  (save-excursion
    (let ((dep-list (ui5-get-dependency-list)))
      (ui5-write-dependencies-and-params dep-list))))

(defun ui5-goto-dependencies ()
  "Goto dependencies position, push a xref marker."
  (interactive)
  (xref-push-marker-stack)
  (goto-char (plist-get (ui5-find-dependency-and-param-area) :dep-end))
  (beginning-of-line 0))

(defun ui5--import-internal (class-name)
  "Import `CLASS-NAME' as dependency."
  (save-excursion
    (let ((dep-list (ui5-get-dependency-list)))
      (print dep-list)
      (unless (string-equal class-name ui5-jq-ns)
        (setq class-name (s-replace "." "/" class-name)))
      (add-to-list 'dep-list class-name)
      (print dep-list)
      (ui5-write-dependencies-and-params dep-list))))

(defun ui5-import ()
  "Select a class to import."
  (interactive)
  (if (region-active-p)
      (ui5--import-internal (buffer-substring-no-properties (region-beginning)
                                                            (region-end)))
    (ivy-read "import a class: " ui5-class-list
              :initial-input (concat " " (thing-at-point 'symbol))
              :action 'ui5--import-internal
              :history 'ui5-import-history
              :keymap counsel-mode-map)))

(defun ui5-import-and-shorten-region ()
  "Import and shorten, only in expression area."
  (interactive)
  (save-excursion
    (let* ((target (buffer-substring-no-properties (region-beginning) (region-end)))
           (short-name (-last-item (s-split "\\." target))))
      (ignore-errors
        (let ((current-prefix-arg '(4)))
          (call-interactively 'jester/evil-multiedit-match-all)))
      (evil-multiedit--substitute)
      (insert short-name)
      (evil-multiedit-abort)
      (ui5--import-internal target))))

(defun ui5--goto-suspect-internal ()
  "Internal function for going to next suspect."
  (re-search-forward (rx (sequence word-start
                                   "sap"
                                   (1+ (sequence "." (1+ (or letter digit "_"))))
                                   word-end))))

(defun ui5-goto-next-suspect ()
  "Goto next suspect looking like it needs to be shorten."
  (interactive)
  (save-match-data
    (deactivate-mark)
    (ui5--goto-suspect-internal)
    ;; (while (not (jester/in-expression-area-p))
    ;;   (ui5--goto-suspect-internal))
    (goto-char (match-beginning 0))
    (evil-visual-char)
    (goto-char (match-end 0))
    (backward-char)))

;; (defun ui5-import-and-shorten-semi-auto ()
;;   "If region active, import it and shorten every occurence in expression area. If not, try to mark next suspect and ask y/n."
;;   (interactive)
;;   (save-match-data
;;     (while (and (re-search-forward (rx (sequence word-start
;;                                                  "sap"
;;                                                  (1+ (sequence "." (1+ letter)))
;;                                                  word-end)))
;;                 ;; (jester/in-expression-area-p)
;;                 )
;;       (let ((target (match-string 0))))
;;       (when (y-or-n-p "import the marked region?")
;;         (evil-multiedit--start-regexp regexp beg end)
;;         ))))

(jester/with-major-leader 'js2-mode-map
                          "u o" 'ui5-organize-dependencies
                          "u g" 'ui5-goto-dependencies
                          "u i" 'ui5-import
                          "n" 'ui5-goto-next-suspect
                          "f" 'ui5-import-and-shorten-region)
(general-define-key
 :keymaps 'js2-mode-map
 "H-n" 'ui5-goto-next-suspect
 "H-f" 'ui5-import-and-shorten-region)


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
