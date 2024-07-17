;;; init-tfm.el --- tabs and file management configuration
;;; Commentary:
;;; 
;;; a number of other files.

;;; code:


(use-package centaur-tabs
		:ensure t
		:disabled
		:demand
		:config
		(centaur-tabs-mode t)
		;; (centaur-tabs-headline-match)
		(setq centaur-tabs-style "bar")
		;; (setq centaur-tabs-modified-marker "*")
		(setq centaur-tabs-label-fixed-length 8)
		(setq centaur-tabs-height 20)
		(if (display-graphic-p) (setq centaur-tabs-set-icons t))
		(setq centaur-tabs-set-bar 'left)
		;; Note: If you're not using Spacmeacs, in order for the underline to display
		;; correctly you must add the following line:
		(setq x-underline-at-descent-line t)
		;; (setq centaur-tabs-set-close-button nil) ;; Uncomment it if you wanna disable the close button.
		(setq centaur-tabs-set-modified-marker t)
		(defun centaur-tabs-hide-tab (x)
		  "Do no to show buffer X in tabs."
		  (let ((name (format "%s" x)))
			(or
			 ;; Current window is not dedicated window.
			 (window-dedicated-p (selected-window))
			 ;; Buffer name not match below blacklist.
			 (string-prefix-p "*epc" name)
			 (string-prefix-p "*helm" name)
			 (string-prefix-p "*Helm" name)
			 (string-prefix-p "*Compile-Log*" name)
			 (string-prefix-p "*lsp" name)
			 (string-prefix-p "*company" name)
			 (string-prefix-p "*Flycheck" name)
			 (string-prefix-p "*tramp" name)
			 (string-prefix-p "*Mini" name)
			 (string-prefix-p "*help" name)
			 (string-prefix-p "*straight" name)
			 (string-prefix-p " *temp" name)
			 (string-prefix-p "*Help" name)
			 (string-prefix-p "*mybuf" name)
			 (string-prefix-p "*Calc" name)
			 (string-prefix-p "*dashboard" name)
			 (string-prefix-p "*Python" name)
			 (string-prefix-p "*" name)
			 ;; Is not magit buffer.
			 (and (string-prefix-p "magit" name)
				  (not (file-name-extension name)))
			 )))
		;; (defun centaur-tabs-buffer-groups ()
		;;   "Officially recommended tabs groups configuration"
		;;   (list
		;;    (cond
		;; 	((or (string-equal "*" (substring (buffer-name) 0 1))
		;; 		 (memq major-mode '(magit-process-mode
		;; 							magit-status-mode
		;; 							magit-diff-mode
		;; 							magit-log-mode
		;; 							magit-file-mode
		;; 							magit-blob-mode
		;; 							magit-blame-mode
		;; 							)))
		;; 	 "Emacs")
		;; 	((derived-mode-p 'prog-mode)
		;; 	 "Editing")
		;; 	((derived-mode-p 'dired-mode)
		;; 	 "Dired")
		;; 	((memq major-mode '(helpful-mode
		;; 						help-mode))
		;; 	 "Help")
		;; 	((memq major-mode '(org-mode
		;; 						org-agenda-clockreport-mode
		;; 						org-src-mode
		;; 						org-agenda-mode
		;; 						org-beamer-mode
		;; 						org-indent-mode
		;; 						org-bullets-mode
		;; 						org-cdlatex-mode
		;; 						org-agenda-log-mode
		;; 						diary-mode))
		;; 	 "Orgs")
		;; 	((memq major-mode '(pdf-view-mode))
		;; 	 "PDF")
		;; 	(t
		;; 	 (centaur-tabs-get-group-name (current-buffer))))))
		;; (centaur-tabs-enable-buffer-alphabetical-reordering)
		; (setq centaur-tabs-adjust-buffer-order t)
		(centaur-tabs-group-by-projectile-project)
		:bind
		;; ("C-<prior>" . centaur-tabs-backward)
		;; ("C-<next>" . centaur-tabs-forward)
		("C-c t" . centaur-tabs-ace-jump)
		("C-c C-t" . centaur-tabs-counsel-switch-group))


(use-package dirvish
  :ensure t
  :init
  (if *is-a-mac* (dirvish-override-dired-mode))
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                          "Home")
	 ("d" "~/Downloads/"                "Downloads")
	;("m" "/mnt/"                       "Drives")
	 ("e" "~/.emacs.d/"                 "emacs.d")
	 ("p" "~/projects/"                 "Projects")
     ("t" "~/.Trash/"                   "TrashCan")
	 ("o" "~/org/"                      "Org notes")
			))
  :config
  (setq centaur-tabs-icon-type 'nerd-icons)
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (if (not *is-a-mac*) (dirvish-override-dired-mode))
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (if (display-graphic-p)
	  (setq dirvish-attributes
			'(vc-state subtree-state nerd-icons git-msg file-time file-size))
	(setq dirvish-attributes
        '(file-time file-size collapse vc-state git-msg)))
  (setq delete-by-moving-to-trash t)
  (if *is-a-mac* (setq dired-listing-switches "-l -h")
	(setq dired-listing-switches
          "-l --human-readable --group-directories-first --no-group"))
  (add-hook 'dired-mode (lambda () (display-line-numbers-mode -1)))
  (setq dirvish-preview-disabled-exts '("iso" "bin" "exe" "gpg" "elc" "eln"))
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   ("C-x d" . dirvish)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ;("y"   . dirvish-yank-menu)
   ;("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("~"   . counsel-find-file)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ; ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("C-b" . dired-up-directory)
   ("C-f" . dired-find-file)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ;; ("M-j" . dirvish-fd-jump))
   ))

(use-package buffer-move
  :ensure t
  :config
  (global-set-key (kbd "C-c <left>") 'buf-move-left)
  (global-set-key (kbd "C-c <right>") 'buf-move-right)
  (global-set-key (kbd "C-c <up>") 'buf-move-up)
  (global-set-key (kbd "C-c <down>") 'buf-move-down))

(provide 'init-tfm)
;;; init-tfm.el ends here
