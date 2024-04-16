;;; init-org.el -- Org mode setup
;;; Commentary:
;;;    

;;; Code:

(use-package org
  
  :config
  (defun swk/init-org-hook ()
	(org-toggle-pretty-entities))
  (add-hook 'org-mode-hook
			(lambda () (local-set-key (kbd "C-j") nil)))
  (defun disable-angle-bracket-in-org-mode ()
	(setq-local electric-pair-pairs (remq '(?< . ?>) electric-pair-pairs)))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t)
  (setq org-image-actual-width '(500))
  :hook
  (org-mode-hook . yas-minor-mode)
  (org-mode-hook . flycheck-mode)
  (org-mode-hook . disable-angle-bracket-in-org-mode)
  ;(org-mode-hook . swk/init-org-hook)
  )
(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(provide 'init-org)

;;; init-org.el ends here
