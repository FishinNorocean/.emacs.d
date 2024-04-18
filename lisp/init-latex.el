;;; init-latex -- Configure LaTeX mode
;;; Commentary:
;;; Code:

;; for Chinese word segmentation
;; https://github.com/kanglmf/emacs-chinese-word-segmentation
;; (require 'init-chinese-word-segment)


(add-hook 'latex-mode-hook
		  (lambda () (local-set-key (kbd "C-j") nil)))
(add-hook 'TeX-mode-hook (lambda ()
						   (setq TeX-auto-save t)
						   (setq TeX-parse-self t)
						   (setq-default TeX-master nil)
						   (keyboard-translate ?¥ ?$)
						   (keyboard-translate ?· ?`)
						   (keyboard-translate ?～ ?~)
						   (keyboard-translate ?、 ?\\)
						   (keyboard-translate ?｜ ?、)
						   (keyboard-translate ?\「 ?{)
						   (keyboard-translate ?\」 ?})
						   (keyboard-translate ?\《 ?<)
						   (keyboard-translate ?\》 ?>)))
(add-hook 'markdown-mode-hook (lambda ()
								(keyboard-translate ?¥ ?$)
								(keyboard-translate ?· ?`)
								(keyboard-translate ?～ ?~)
								(keyboard-translate ?、 ?\\)
								(keyboard-translate ?｜ ?、)
								(keyboard-translate ?\「 ?{)
								(keyboard-translate ?\」 ?})
								(keyboard-translate ?\《 ?<)
								(keyboard-translate ?\》 ?>)))
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

(use-package company-auctex
  :ensure t
  :config
  (company-auctex-init))

(with-eval-after-load 'latex
  (define-key LaTeX-mode-map (kbd "C-<return>") 'tex-insert-macro)
  (define-key LaTeX-mode-map (kbd "C-j") nil)
  (define-key LaTeX-mode-map (kbd "C-j C-SPC") 'avy-goto-char-timer)
  (define-key LaTeX-mode-map (kbd "C-j C-k") 'avy-move-line)
  (define-key LaTeX-mode-map (kbd "C-j M-k") 'avy-kill-ring-save-whole-line)
  (define-key LaTeX-mode-map (kbd "C-j C-l") 'avy-copy-line)
  (define-key LaTeX-mode-map (kbd "C-j C-i") 'avy-copy-region)
  (define-key LaTeX-mode-map (kbd "C-j C-w") 'avy-kill-ring-save-region)
  (define-key LaTeX-mode-map (kbd "C-j M-w") 'avy-kill-region))


(add-hook 'latex-mode
			(lambda () (local-set-key (kbd "C-j") nil)))

(use-package pdf-tools
  :if (display-graphic-p)
  :ensure t
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(defun pdf-view-kill-rmn-ring-save ()
  "Copy the region to the `kill-ring' after remove all newline characters."
  (interactive)
  (pdf-view-assert-active-region)
  (let* ((txt (replace-regexp-in-string "\n" " "
										(car (pdf-view-active-region-text)))))
    (pdf-view-deactivate-region)
	(kill-new txt)))

(use-package pdf-view-mode
  :bind
  ("C-c C-w" . pdf-view-kill-rmn-ring-save)
  :config
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))
  

(provide 'init-latex)

;;; init-latex.el ends here
