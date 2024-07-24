;;; init-preload-local -- Configure some basic function and keyboard shortcuts
;;; Commentary:
;;; Code:

(setq confirm-kill-emacs #'yes-or-no-p)
(set-selection-coding-system 'utf-8)
(setq-default tab-width 4)
(electric-pair-mode t)
(setq initial-scratch-message "Rock\'n\'Roll ain't noise pollution.")
(add-hook 'prog-mode-hook #'show-paren-mode)
(column-number-mode t)
(if *is-a-mac* (setq split-width-threshold 120))
(global-auto-revert-mode t)
(delete-selection-mode t)
;; (setq shell-command-prompt-show-cwd nil)
;; (setq eshell-echo-input nil)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)    ;; Hide the startup message
(setq-default python-indent 4)
(setq c-basic-offset 4)
;;(setq-default indent-tabs-mode nil)
(setq make-backup-files nil) ; stop creating backup~ files
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; (add-hook 'fundamental-mode-hook (lambda () (display-line-numbers-mode -1)))
(global-display-line-numbers-mode 1)

(setq display-line-numbers-type 'relative)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(desktop-save-mode -1)
(global-hl-line-mode t)
;; (global-tab-line-mode)
(when (display-graphic-p) (toggle-scroll-bar -1))
;; (add-to-list 'default-frame-alist '(width . 90))
;; (add-to-list 'default-frame-alist '(height . 55))
(toggle-frame-maximized)
(when *is-a-mac* (toggle-frame-fullscreen))

;; (put 'scroll-left 'disabled nil)

(savehist-mode 1)
(setq savehist-file "~/.emacs.d/.savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(setq enable-remote-dir-locals t)

(global-set-key (kbd "C-x M-b") 'switch-to-buffer)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c w") 'swk/web-quick-access)
;; (global-set-key (kbd "C-t C-e") 'centaur-tabs--kill-this-buffer-dont-ask)

;;; keyboard setting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Set home to Hyper
(global-set-key (kbd "<home>") nil)
(define-key function-key-map (kbd "<home>") 'event-apply-hyper-modifier)

;; Set end to super
(global-set-key (kbd "<end>") nil)
(define-key function-key-map (kbd "<end>") 'event-apply-super-modifier)

;; (global-set-key [wheel-right] 'scroll-left)
;; (global-set-key [wheel-left] 'scroll-right)

;; Shortcuts
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
;;(global-set-key (kbd "C-a") 'back-to-indentation) ;; swap C-a and M-m
;;(global-set-key (kbd "M-m") 'move-beginning-of-line)
;;(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-h") 'hs-hide-block)
(global-set-key (kbd "H-s") 'hs-show-block)
(cond ((string-equal system-type "darwin")
       (progn
         ;; modify option and command key
         (setq mac-command-modifier 'meta)
         (setq mac-option-modifier 'super)
		 )))
(global-unset-key (kbd "C-t"))
(global-set-key (kbd "C-t C-t") 'swk/new-terminal)
(global-set-key (kbd "C-t C-s") 'toggle-window-split)

(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(add-hook 'after-init-hook
          (lambda ()
            (kill-buffer-if-exists "*scratch*")))

;; Faster move cursor
(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

;; (global-set-key (kbd "C-x C-y") 'pbpaste)
;; (global-set-key (kbd "C-x C-w") 'pbcopy)

;; bookmark
(global-set-key (kbd "H-x m") 'bookmark-set)
(global-set-key (kbd "H-x b") 'bookmark-jump)
(global-set-key (kbd "H-x l") 'bookmark-bmenu-list)

;; lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-region)

;; hippie
; (global-set-key (kbd "C-<tab>") 'hippie-expand)


;; shell remove escape characters
;; (defun preamble-regexp-alternatives (regexps)
;;   "Return the alternation of a list of regexps."
;;   (mapconcat (lambda (regexp)
;;                (concat "\\(?:" regexp "\\)"))
;;              regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(defun regexp-alternatives (regexps)
  (mapconcat (lambda (regexp) (concat "\\(" regexp "\\)")) regexps "\\|"))

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

(put 'dired-find-alternate-file 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'markdown-mode-hook (lambda ()
						   (keyboard-translate ?· ?`)))
;; Faster move cursor
(global-set-key (kbd "M-n") 'next-ten-lines)
(global-set-key (kbd "M-p") 'previous-ten-lines)

(provide 'init-preload-local)
;;; init-preload-local.el ends here
