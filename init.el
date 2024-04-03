;;; init.el --- Init Emacs frome here
;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Code:
(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
	(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
		  (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Bootstrap config
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
;; (setq package-native-compile t)

;; use-package has been integrated since emacs 29
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "elpa/use-package-2.4.1/")
  (require 'use-package))

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)
(require 'init-themes)

