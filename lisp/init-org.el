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

  :hook
  (org-mode-hook . yas-minor-mode)
  (org-mode-hook . flycheck-mode)
  (org-mode-hook . disable-angle-bracket-in-org-mode)
  ;(org-mode-hook . swk/init-org-hook)
  )

(provide 'init-org)

;;; init-org.el ends here
