;;; init-vc.el --- version control plugins setting
;;; Commentary:
;;; 

;;; code:



(use-package magit
  :ensure t)

;;; Setting up the fringe

(use-package fringe-helper
  :ensure t
  :config
  (defconst doom-fringe-size '3 "Default fringe width")
  ;; switches order of fringe and margin
  (setq-default fringes-outside-margins t)
  ;; standardize fringe width
  (fringe-mode doom-fringe-size)
  (push `(left-fringe  . ,doom-fringe-size) default-frame-alist)
  (push `(right-fringe . ,doom-fringe-size) default-frame-alist))

;;; Setting up git-gutter

(use-package git-gutter
  :ensure t
  :config
  (use-package git-gutter-fringe
	:ensure t)
  ;; colored fringe "bars"
  (define-fringe-bitmap 'git-gutter-fr:added
	[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
	[224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
	nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
	[0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
	nil nil 'center)
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:deleted "red")
  (setq git-gutter:update-interval 2)
  :hook
  (prog-mode . git-gutter-mode)
  (text-mode . git-gutter-mode)
  (conf-mode . git-gutter-mode))




(provide 'init-vc)
;;; init-vc.el ends here
