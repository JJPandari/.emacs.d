;; -*- lexical-binding: t -*-
;; (setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; https://github.com/seagle0128/.emacs.d/issues/37
;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "extensions" user-emacs-directory) load-path)
  (push (expand-file-name "modes" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)
              (run-with-idle-timer 30 t 'garbage-collect)
              (add-hook 'focus-out-hook 'garbage-collect))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-utils)
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-gui-frames)

(require 'init-osx-keys)
(require 'init-which-key)
(require 'init-evil)

(require 'init-editing-utils)

(require 'init-uniquify)
(require 'init-dired)
(require 'init-file)
(require 'init-buffer)
(require 'init-minibuffer)
(require 'init-flycheck)

(require 'init-ivy)
(require 'init-company)
(require 'init-yas)
(require 'init-windows)
(require 'init-mode-line)
(require 'init-sessions)
(require 'init-fonts)

(require 'init-vc)
(require 'init-git)

(require 'init-eyebrowse)
(require 'init-avy)

(require 'init-dumbjump)
;; (require 'init-compile)
;; (require 'init-org)
(require 'init-markdown)
(require 'init-csv)
(require 'init-web)
(require 'init-javascript)
(require 'init-json)
(require 'init-coffeescript)
(require 'init-css)
(require 'init-http)
;; (require 'init-python)
;; ;; (require 'init-haskell)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
(require 'init-yaml)
;; (require 'init-docker)
;; ;; (maybe-require-package 'nginx-mode)
(require 'init-lua)
(require 'init-dotenv)

(require 'init-paredit)
(require 'init-lisp)
;; (require 'init-scheme)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)
;; (require 'init-common-lisp)

;; TODO pyim
(require 'init-youdao)
(require 'init-keyfreq)
(require 'init-edit-server)
(require 'init-misc)

;; Extra packages which don't require any configuration

;; location for calander etc.
(when *is-a-mac*
  (require-package 'osx-location))
;; services at system startup
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
