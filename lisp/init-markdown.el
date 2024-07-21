;;; init-markdown.el -- markdown support setup
;;; Commentary:
;;;    

;;; Code:

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

(provide 'init-markdown)

;;; init-markdown.el ends here
