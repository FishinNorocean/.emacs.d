;;; init-stat.el --- Configurations about statistic softwares.
;;; Commentary:

;;; Code:

(use-package ess
  :ensure t
  :config
  (setq pyvenv-virtual-env-name "R")
  (defun venv-ess-eval-paragraph-and-step ()
	"Helo you settle the venv probelm launching R."
	(interactive)
	(setq inferior-ess-r-program
	  (concat "~/.local/miniconda3/envs/" pyvenv-virtual-env-name  "/bin/R"))
	(ess-eval-paragraph-and-step))
  :bind
  (:map ess-r-mode-map
        ("C-c C-p" . venv-ess-eval-paragraph-and-step)))
  
  

(provide 'init-stat)

;;; init-stat.el ends here
