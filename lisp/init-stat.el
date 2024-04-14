;;; init-stat.el --- Configurations about statistic softwares.
;;; Commentary:

;;; Code:

(use-package ess
  :ensure t
  :config
  (setq inferior-ess-r-program
		(concat "~/.local/miniconda3/envs/" pyvenv-virtual-env-name  "/bin/R")))
  

(provide 'init-stat)

;;; init-stat.el ends here
