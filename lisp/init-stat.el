;;; init-stat.el --- Configurations about statistic softwares.
;;; Commentary:

;;; Code:

(use-package ess
  :ensure t
  :config
  (setq inferior-ess-r-program "/Users/swk/.local/miniconda3/envs/R/bin/R"))

(provide 'init-stat)

;;; init-stat.el ends here
