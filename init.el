;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "extensions" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modes" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(require 'init-utils)
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-gui-frames)

(require 'init-osx-keys)
;; ;; TODO here
;; TODO test minimal init-evil
(require 'init-evil)

;; (require 'init-editing-utils)
;; (require 'init-whitespace)

;; (require 'init-dired)
;; (require 'init-isearch)
;; (require 'init-grep)
;; (require 'init-uniquify)
;; (require 'init-ibuffer)
;; (require 'init-flycheck)

;; (require 'init-recentf)
;; (require 'init-smex)
;; (require 'init-ivy)
;; (require 'init-hippie-expand)
;; (require 'init-company)
;; (require 'init-windows)
;; (require 'init-sessions)
;; (require 'init-fonts)
;; (require 'init-mmm)

;; (require 'init-vc)
;; (require 'init-git)
;; (require 'init-github)

;; (require 'init-projectile)

;; ;; TODO add here maybe??? JJ
;; (require 'init-eyebrowse)

;; (require 'init-compile)
;; (require 'init-org)
;; (require 'init-markdown)
;; (require 'init-csv)
;; (require 'init-web)
;; (require 'init-javascript)
;; (require 'init-nxml)
;; (require 'init-html)
;; (require 'init-css)
;; (require 'init-http)
;; (require 'init-python)
;; ;; (require 'init-haskell)
;; (require 'init-ruby)
;; (require 'init-rails)
;; (require 'init-sql)
;; (require 'init-yaml)
;; (require 'init-docker)
;; ;; (maybe-require-package 'nginx-mode)

;; (require 'init-paredit)
;; (require 'init-lisp)
;; ;; (require 'init-slime)
;; ;; (require 'init-clojure)
;; ;; (require 'init-clojure-cider)
;; ;; (require 'init-common-lisp)

;; (require 'init-misc)

;; ;; Extra packages which don't require any configuration

;; ;; TODO defer using auto mode
;; (require-package 'lua-mode)
;; ;; location for calander etc.
;; (when *is-a-mac*
;;   (require-package 'osx-location))
;; ;; services at system startup
;; (unless (eq system-type 'windows-nt)
;;   (maybe-require-package 'daemons))
;; (maybe-require-package 'dotenv-mode)

;; (when (maybe-require-package 'uptimes)
;;   (setq-default uptimes-keep-count 200)
;;   (add-hook 'after-init-hook (lambda () (require 'uptimes))))


;; ;;----------------------------------------------------------------------------
;; ;; Allow access from emacsclient
;; ;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; ;;----------------------------------------------------------------------------
;; ;; Variables configured via the interactive 'customize' interface
;; ;;----------------------------------------------------------------------------
;; (setq custom-file "./custom.el")
;; (unless (file-exists-p custom-file)
;;   (write-region "" nil custom-file))
;; (when (file-exists-p custom-file)
;;   (load custom-file))


;; ;;----------------------------------------------------------------------------
;; ;; Locales (setting them earlier in this file doesn't work in X)
;; ;;----------------------------------------------------------------------------
;; (require 'init-locales)


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
