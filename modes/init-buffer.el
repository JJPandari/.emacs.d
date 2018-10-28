(setq initial-scratch-message "")

(jester/with-leader
 "b b" 'switch-to-buffer
 "b i" 'ibuffer
 "b k" 'jester/kill-this-buffer
 "b r" 'rename-buffer
 "b h" (lambda! (switch-to-buffer (help-buffer)))
 "b m" (lambda! (switch-to-buffer (messages-buffer)))
 "b s" 'jester/switch-to-scratch-buffer
 "<return>" 'jester/alternate-buffer)

(general-define-key
 :keymaps 'ibuffer-mode-map
 "j" 'ibuffer-forward-line
 "k" 'ibuffer-backward-line)

;;----------------------------------------------------------------------------
;; switch *scratch* buffer
;;----------------------------------------------------------------------------
(defun jester/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))

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
;; kill this buffer.
;;----------------------------------------------------------------------------
(defun jester/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive)
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

(provide 'init-buffer)
