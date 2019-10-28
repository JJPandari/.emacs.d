(require 'package)


;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))



;;; Standard package repositories

(setq package-archives
      (append '(
                ;; ("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
                ;; ("org-cn" . "https://elpa.emacs-china.org/org/")
                ;; ("gnu-cn" . "https://elpa.emacs-china.org/gnu/")
                ;; ("melpa-stable-cn" . "https://elpa.emacs-china.org/melpa-stable/")
                ("melpa-tuna" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                ("org-tuna" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                ("gnu-tuna" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                ("melpa-stable-tuna" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
                )
              package-archives))



;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;;; Fire up package.el

(setq package-enable-at-startup nil)
(package-initialize)



(require-package 'fullframe)
(fullframe list-packages quit-window)


(require-package 'cl-lib)
(require 'cl-lib)

(defun jester/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
             when (string= col-name (car column))
             do (setf (elt column 1) width))))

(defun jester/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (jester/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (jester/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'jester/maybe-widen-package-menu-columns)


(setq use-package-always-ensure t
      use-package-always-defer t)
(require-package 'use-package)
(require 'use-package)


(provide 'init-elpa)
