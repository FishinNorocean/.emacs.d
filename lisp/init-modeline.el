;;; init-modeline.el --- modeline configuration
;;; Commentary:
;;; config a good looking mode line
;;; a number of other files.

;;; code:

;; sml-mode -- smart mode line
(use-package smart-mode-line
  :if (not (display-graphic-p))
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  :config
  (setq display-time-format "%a %b %d %R")
  (display-time-mode 1)
  (setq rm-blacklist
		(format "^ \\(%s\\)$"
				(mapconcat #'identity
                           '("Projectile.*" "company.*" "Google"
							 "Undo-Tree" "counsel" "ivy" "yas" "WK")
                           "\\|")))
  ; (setq sml/use-projectile-p 'before-prefixes)
  ; (setq sml/all-the-icons-symlink t)
  (sml/setup))

(use-package nyan-mode
  :if (display-graphic-p)
  :ensure t
  :config
  (nyan-mode)
  (setq nyan-animate-nyancat t)
  (nyan-start-animation)
  (setq nyan-cat-face-number 1)
  ; (setq nyan-bar-length 20)
  (setq nyan-wavy-trail t))


(use-package doom-modeline
  :if (display-graphic-p)
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-total-line-number t)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-time t)
  (setq doom-modeline-battery t)
  ; (setq display-time-format "%a %b %d %R")
  (setq display-time-default-load-average nil)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (display-time-mode 1)
  ; (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python t))

(provide 'init-modeline)
;;; init-modeline.el ends here
