;;; init-org.el -- Org mode setup
;;; Commentary:
;;;    

;;; Code:

(use-package org
  
  :config
  (defun swk/init-org-hook ()
	(org-toggle-pretty-entities))
  :hook
  (org-mode-hook . yas-minor-mode)
  ;(org-mode-hook . swk/init-org-hook)
  )

(provide 'init-org)

;;; init-org.el ends here
