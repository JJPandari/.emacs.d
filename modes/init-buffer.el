(jester/with-leader
 "b b" 'switch-to-buffer
 "b i" 'ibuffer
 "b k" 'jester/kill-this-buffer
 "b r" 'rename-buffer
 "b q" 'bury-buffer
 "b h" (lambda! (switch-to-buffer (help-buffer)))
 "b m" (lambda! (switch-to-buffer (messages-buffer)))
 "b s" 'jester/switch-to-scratch-buffer
 "b S" 'jester/scratch-from-current-buffer
 "b y" 'jester/copy-buffer-name
 "<return>" 'jester/alternate-buffer
 "m s" 'jester/switch-mode)

;;----------------------------------------------------------------------------
;; setup ibuffer.
;;----------------------------------------------------------------------------
(defun ibuffer-set-up-preferred-filters ()
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

;;----------------------------------------------------------------------------
;; switch to *scratch* buffer
;;----------------------------------------------------------------------------
(defun jester/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (if arg
      (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (switch-to-buffer (get-buffer-create "*scratch*")))
  (lisp-interaction-mode))

;;----------------------------------------------------------------------------
;; switch to last buffer
;;----------------------------------------------------------------------------
(defun jester/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

;;----------------------------------------------------------------------------
;; kill this buffer.
;;----------------------------------------------------------------------------
(defun jester/kill-this-buffer (&optional arg)
  "Kill the current buffer."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

;;----------------------------------------------------------------------------
;; copy the buffer name.
;;----------------------------------------------------------------------------
(defun jester/copy-buffer-name ()
  "Copy the buffer name, show it in minibuffer."
  (interactive)
  (message (kill-new (buffer-name))))

;;----------------------------------------------------------------------------
;; switch major mode
;;----------------------------------------------------------------------------
;; TODO standalone history with `ivy-read'
(defun jester/switch-mode ()
  "Toggle some mode."
  (interactive)
  (minibuffer-with-setup-hook #'beginning-of-line
    (counsel-M-x " -mode$")))

;;----------------------------------------------------------------------------
;; copy buffer content and do some experiments
;;----------------------------------------------------------------------------
(defun jester/scratch-from-current-buffer ()
  "Copy current buffer content, make a new buffer to fiddle with. If region active, only copy region."
  (interactive)
  (let ((mode major-mode)
        (text (if (region-active-p)
                  (prog1 (buffer-substring (region-beginning) (region-end))
                    (deactivate-mark))
                (buffer-substring (point-min) (point-max)))))
    (switch-to-buffer (generate-new-buffer (format "fiddling: %s" (buffer-name))))
    (insert text)
    (funcall mode)))

(provide 'init-buffer)
