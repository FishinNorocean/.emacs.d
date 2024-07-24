;;; init-vc.el --- version control plugins setting
;;; Commentary:
;;; 

;;; code:



(use-package magit
  :ensure t)

;;; Setting up the fringe

(use-package fringe-helper
  :disabled
  :ensure t
  :config
  (defconst doom-fringe-size '14 "Default fringe width")
  ;; switches order of fringe and margin
  (setq-default fringes-outside-margins t)
  ;; standardize fringe width
  (fringe-mode doom-fringe-size)
  (push `(left-fringe  . ,doom-fringe-size) default-frame-alist)
  (push `(right-fringe . ,doom-fringe-size) default-frame-alist))

(set-fringe-style  '(12 . 8))

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
	[0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248])
  ;; (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  ;; (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
  ;; (custom-set-variables
  ;;  '(git-gutter:modified-sign "|") ;; two space
  ;;  '(git-gutter:added-sign "|")    ;; multiple character is OK
  ;;  '(git-gutter:deleted-sign "|"))
  (set-face-foreground 'git-gutter-fr:added "#0F9D58")
  (set-face-foreground 'git-gutter-fr:modified "#4285F4")
  (set-face-foreground 'git-gutter-fr:deleted "#DB4437")
  (setq git-gutter:update-interval 2)
  ;(if (not *is-a-mac*) (fringe-mode '(15 . 0)))
  :hook
  (prog-mode . git-gutter-mode)
  (text-mode . git-gutter-mode)
  (conf-mode . git-gutter-mode))

(use-package diff-hl
  :disabled
  :ensure t
  ;; :init
  ;; (global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-flydiff-delay 0.5)
  :hook
  (prog-mode . git-gutter-mode)
  (dired-mode . git-gutter-mode)
  (text-mode . git-gutter-mode)
  (conf-mode . git-gutter-mode))


(provide 'init-vc)
;;; init-vc.el ends here
