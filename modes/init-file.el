(jester/with-leader
 "f f" 'find-file
 "f s" 'save-buffer
 "f S" 'evil-write-all
 "f D" 'jester/delete-buffer-file
 "b D" 'jester/delete-buffer-file
 "f R" 'jester/rename-buffer-file
 "f y" 'jester/show-and-copy-buffer-filename)

(setq auto-save-default nil)

(defvar jester-auto-save-idle 1 "Time in seconds before auto-saving all buffers.")
(run-with-idle-timer jester-auto-save-idle t #'jester/save-all-buffers)
;; (cancel-function-timers 'jester/save-all-buffers) ;; for debugging
(add-hook 'focus-out-hook #'jester/save-all-buffers)

;;----------------------------------------------------------------------------
;; Save all buffers.
;;----------------------------------------------------------------------------
(defun jester/save-all-buffers ()
  "Save all buffers."
  ;; yas overlay and company-select-next has problem with this.
  (when (and (not yas--active-snippets)
             (not company-candidates)
             (not (eq major-mode 'snippet-mode)))
    ;; https://github.com/manateelazycat/lazycat-emacs/commit/da13a688ef89f8ab2c577a3e9d2a7bcf0ef9b71d
    (with-temp-message
       (with-current-buffer " *Minibuf-0*" (buffer-string))
     (save-some-buffers t #'(lambda () (and (buffer-file-name) (buffer-modified-p)))))))

;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun jester/delete-buffer-file ()
  "Trash the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name) t)
    (kill-this-buffer)))

;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun jester/rename-buffer-file ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (let ((new-name (read-from-minibuffer "rename file to: " (file-name-nondirectory filename))))
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (set-visited-file-name new-name)
        (rename-buffer new-name)))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun jester/browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;----------------------------------------------------------------------------
;; check when opening large files - literal file open
;;----------------------------------------------------------------------------
(defconst jester-large-file-size-MB 1)
(defvar jester-large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode))

(defun jester/check-large-file ()
  (let* ((filename (buffer-file-name))
         (size (nth 7 (file-attributes filename))))
    (when (and
           (not (memq major-mode jester-large-file-modes-list))
           size (> size (* 1024 1024 jester-large-file-size-MB))
           (y-or-n-p (format (concat "%s is a large file, open literally to "
                                     "avoid performance issues?")
                             filename)))
      (setq buffer-read-only t)
      (buffer-disable-undo)
      (fundamental-mode))))

(add-hook 'find-file-hook 'jester/check-large-file)

;;----------------------------------------------------------------------------
;; sudo edit file
;;----------------------------------------------------------------------------
(defun jester/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop
                                                     ))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
                                                       parsed-localname
                                                       new-hop)))
           new-fname))))))

;;----------------------------------------------------------------------------
;; show file path
;;----------------------------------------------------------------------------
;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun jester/show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))


(provide 'init-file)
