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
(defconst jester-submodules-dir (expand-file-name "extensions" user-emacs-directory))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)
              (run-with-idle-timer 30 t 'garbage-collect)
              ;; (add-hook 'focus-out-hook 'garbage-collect)
              )))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
;; Calls (package-initialize)
(require 'init-elpa)
(require 'init-straight)
(require 'init-utils)
(require 'init-exec-path)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-emacs-server) ; need to put this first to know which instance/server we are in, for initing other features

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-gui-frames)
(require 'init-face)
;; (require 'init-eaf)

(require 'init-osx-keys)
(require 'init-evil)
;; (require 'init-meow)
(require 'init-which-key)
(require 'init-hydra)
(require 'init-multi-cursors)

(require 'init-editing-utils)

(require 'init-uniquify)
(require 'init-dired)
(require 'init-file)
(require 'init-buffer)
(require 'init-minibuffer)

;; (require 'init-helm)
(require 'init-ivy)
;; (require 'init-selectrum)

;; (require 'init-company)
(require 'init-lsp-bridge)
(require 'init-yas)

(require 'init-windows)
(require 'init-mode-line)
(require 'init-sessions)
(require 'init-avy)
(require 'init-help)
(require 'init-read)

(require 'init-flycheck)
(require 'init-vc)
(require 'init-git)
(require 'init-compile)
(require 'init-formatter)

(require 'init-tree-sitter)
(require 'init-paredit)

(require 'init-lisp)
(require 'init-scheme)
;; (require 'init-slime)
;; (require 'init-clojure)
;; (require 'init-clojure-cider)
;; (require 'init-common-lisp)

(require 'init-projects)
;; (require 'init-lsp)
(require 'init-jump)
(require 'init-text)
(require 'init-org)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-web)
(require 'init-json)
(require 'init-coffeescript)
(require 'init-css)
(require 'init-java)
;; (require 'init-python)
;; (require 'init-haskell)
(require 'init-elm)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-php)
;; (require 'init-sql)
(require 'init-rust)
(require 'init-yaml)
(require 'init-docker)
(require 'init-nginx)
(require 'init-lua)
(require 'init-ahk)
(require 'init-dotenv)
(require 'init-shell)
(require 'init-http)
(require 'init-cucumber)

(require 'init-chinese)
(require 'init-keyfreq)
(require 'init-edit-server)
(require 'init-browser)
(require 'init-player)
(require 'init-time)
(require 'init-ai)
(require 'init-misc)

;; Extra packages which don't require any configuration

;; location for calander etc.
(when *is-a-mac*
  (require-package 'osx-location))
;; services at system startup
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))


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
