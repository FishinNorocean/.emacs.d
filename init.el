;;; init.el --- Init Emacs frome here
;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Code:
;; (setq debug-on-error t)
(setq init-file-debug nil)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
	(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "mdx-dict" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "copilot" user-emacs-directory))
(setenv "EMACS_ZSH" "true")
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------

(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
		  (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Bootstrap config
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
;; (setq package-native-compile t)

;; use-package has been integrated since emacs 29
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "elpa/use-package-2.4.1/")
  (require 'use-package))

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local)
(require 'init-themes)

;; ===========================================
;; Basic Customization (in init-preload-local)
;; ===========================================

(use-package helpful
  :ensure t
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key)
  ("C-h x" . 'helpful-command))

(use-package hydra
  :ensure t)

(use-package use-package-hydra
  :ensure t
  :after hydra)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-height 10)
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-r" . 'swiper-backward)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(global-set-key (kbd "C-r") 'read-only-mode)

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-width 80)
  (setq ivy-posframe-height 10)
  (if (not *is-a-mac*) (setq ivy-posframe-font "MesloLGS NF 12"))
  ;; (setq ivy-posframe-display-functions-alist
  ;; 		'((swiper          . ivy-posframe-display-at-point)
  ;;         ;(complete-symbol . ivy-posframe-display-at-point)
  ;;         (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
  ;;         (t               . ivy-display-function-fallback)))
  (ivy-posframe-mode 1))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

(require 'init-tfm)

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
	(unwind-protect
		(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
	t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)
   ("C-j C-k" . avy-move-line)
   ("C-j M-k" . avy-kill-ring-save-whole-line)
   ("C-j C-l" . avy-copy-line)
   ("C-j C-i" . avy-copy-region)
   ("C-j C-w" . avy-kill-ring-save-region)
   ("C-j M-w" . avy-kill-region)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package meow
  :disabled
  :ensure t
  :init
  (defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   ; '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode 1)
  (setq delete-active-region t))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package tiny ; m1\n10|int func%02d ()
  :ensure t)

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-require-match nil)
  ;; (setq company-frontends '(company-preview-frontend))
  )

(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key prog-mode-map (kbd "C-<return>") 'copilot-accept-completion)

(use-package company-box
  :ensure t
  :if window-system
  :hook (company-mode . company-box-mode))
  
  

(use-package codeium
  :disabled
  :init
  ;; use globally
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
  ;; or on a hook
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

  ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local completion-at-point-functions
  ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
  ;; an async company-backend is coming soon!

  ;; codeium-completion-at-point is autoloaded, but you can
  ;; optionally set a timer, which might speed up things as the
  ;; codeium local language server takes ~0.2s to start up
  ;; (add-hook 'emacs-startup-hook
  ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

  :defer t
  :config
  (setq use-dialog-box t) ;; do not use popup boxes

  ;; if you don't want to use customize to save the api-key
  ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

  ;; get codeium status in the modeline
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

  ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  ;; You can overwrite all the codeium configs!
  ;; for example, we recommend limiting the string sent to codeium for better performance
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

(use-package company-tabnine
  :disabled
  :ensure t
  :init (add-to-list 'company-backends #'company-tabnine))

(use-package flycheck
  :ensure t
  :init
  :config
  (setq truncate-lines nil)
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode-hook . (lambda () (setq flycheck-clang-language-standard "c++17"))))

(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))


(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Think good. Do better. Be the best.")
  (setq dashboard-projects-backend 'projectile)
  ; (setq dashboard-startup-banner 1)
  (setq dashboard-startup-banner (expand-file-name "marisa.png" user-emacs-directory))
  (setq dashboard-items '((recents  . 5)
						  (projects . 5)
						  (bookmarks . 3)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-footer-messages '("Take good care of yourself."))
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook)
  :bind (:map dashboard-mode-map
         ("d" . 'dirvish)
		 ; ("n" . 'next-line)
		 ; ("p" . 'previous-line)
		 ("g" . 'magit)))

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-project-search-path '("~/projects/"))
  (defadvice projectile-project-root (around ignore-remote first activate)
	(unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :config (counsel-projectile-mode)
  )

;; slime
(setq inferior-lisp-program "sbcl")

(use-package yaml-mode
  :ensure t
  :defer t)

(defun enable-lsp-if-not-remote ()
  (unless (file-remote-p default-directory) (lsp-deferred)))


;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
		lsp-file-watch-threshold 500)
  ;; lsp-prefer-flymake nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :none)
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; (add-to-list 'lsp-clients-clangd-args "--clang-tidy")
  :bind
  ("C-c l s" . lsp-ivy-workspace-symbol))
  ; :commands lsp
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;; 					:major-modes '(python-mode)
  ;; 					:remote? t
  ;; 					:server-id 'pyls-remote))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'top))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :init (add-to-list 'image-types 'svg)
  :commands dap-debug
  :custom
  (dap-auto-configure-mode t)
  :hydra
  (dap-hydra
   (:color pink :hint nil :foreign-keys run)
   "
^Stepping^          ^Switch^                 ^Breakpoints^         ^Debug^                     ^Eval^                      
^^^^^^^^---------------------------------------------------------------------------------------------------------------
_n_: Next           _ss_: Session            _bb_: Toggle          _dd_: Debug                 _ee_: Eval                  
_i_: Step in        _st_: Thread             _bd_: Delete          _dr_: Debug recent          _er_: Eval region
_o_: Step out       _sf_: Stack frame        _ba_: Add             _dl_: Debug last            _es_: Eval thing at point
_c_: Continue       _su_: Up stack frame     _bc_: Set condition   _de_: Edit debug template   _ea_: Add expression.
_r_: Restart frame  _sd_: Down stack frame   _bh_: Set hit count   _ds_: Debug restart
_Q_: Disconnect     _sl_: List locals        _bl_: Set log message
                  _sb_: List breakpoints
                  _se_: List expressions
"
   ("n" dap-next)
   ("i" dap-step-in)
   ("o" dap-step-out)
   ("c" dap-continue)
   ("r" dap-restart-frame)
   ("ss" dap-switch-session)
   ("st" dap-switch-thread)
   ("sf" dap-switch-stack-frame)
   ("su" dap-up-stack-frame)
   ("sd" dap-down-stack-frame)
   ("sl" dap-ui-locals)
   ("sb" dap-ui-breakpoints)
   ("se" dap-ui-expressions)
   ("bb" dap-breakpoint-toggle)
   ("ba" dap-breakpoint-add)
   ("bd" dap-breakpoint-delete)
   ("bc" dap-breakpoint-condition)
   ("bh" dap-breakpoint-hit-condition)
   ("bl" dap-breakpoint-log-message)
   ("dd" dap-debug)
   ("dr" dap-debug-recent)
   ("ds" dap-debug-restart)
   ("dl" dap-debug-last)
   ("de" dap-debug-edit-template)
   ("ee" dap-eval)
   ("ea" dap-ui-expressions-add)
   ("er" dap-eval-region)
   ("es" dap-eval-thing-at-point)
   ("q" nil "quit" :color blue)
   ("Q" dap-disconnect "Disconnect" :color blue))
  :config
  (dap-ui-mode 1)
  (defun dap-hydra ()
	(interactive)
	"Run `dap-hydra/body'."
	(dap-hydra/body)))

(use-package treemacs
  :disabled
  :ensure t
  :defer t
  :config
  (treemacs-tag-follow-mode)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
		("/" . treemacs-advanced-helpful-hydra)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(require 'init-vc)

(use-package yasnippet
  :ensure t
  :init
  ; (if *is-a-mac* (yas-global-mode))
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (defun company-mode/backend-with-yas (backend)
	(if (and (listp backend) (member 'company-yasnippet backend))
		backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  :bind
  (:map yas-minor-mode-map ("C-<tab>" . yas-expand)))

(yas-global-mode)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package which-key
  :disabled
  :ensure t
  :init (which-key-mode))

(use-package highlight-symbol
  :ensure t
  ;; :init (if *is-a-mac* (highlight-symbol-mode))
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.5)  ; 0.5秒后自动高亮相同符号
  )



;; My mode about CALPUFF
;; (load-file "~/.emacs.d/mymode/inp-mode.el")
;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . inp-mode))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  (("C-x C-h m" . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :hydra
  (hydra-multiple-cursors
   (:hint nil)
   "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
   ("l" mc/edit-lines :exit t)
   ("a" mc/mark-all-like-this :exit t)
   ("n" mc/mark-next-like-this)
   ("N" mc/skip-to-next-like-this)
   ("M-n" mc/unmark-next-like-this)
   ("p" mc/mark-previous-like-this)
   ("P" mc/skip-to-previous-like-this)
   ("M-p" mc/unmark-previous-like-this)
   ("|" mc/vertical-align)
   ("s" mc/mark-all-in-region-regexp :exit t)
   ("0" mc/insert-numbers :exit t)
   ("A" mc/insert-letters :exit t)
   ("<mouse-1>" mc/add-cursor-on-click)
   ;; Help with click recognition in this hydra
   ("<down-mouse-1>" ignore)
   ("<drag-mouse-1>" ignore)
   ("q" nil)))



(require 'init-programming)
(require 'init-org)
(require 'init-stat)

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))
;; (if *is-a-mac* 
;;     (use-package undo-tree
;;       :ensure t
;;       :config 
;;       (global-undo-tree-mode)
;;       :after hydra
;;       :hydra (hydra-undo-tree (:hint nil)
;;                               "
;;   _p_: undo  _n_: redo _s_: save _l_: load   "
;;                               ("p"   undo-tree-undo)
;;                               ("n"   undo-tree-redo)
;;                               ("s"   undo-tree-save-history)
;;                               ("l"   undo-tree-load-history)
;;                               ("u"   undo-tree-visualize "visualize" :color blue)
;;                               ("q"   nil "quit" :color blue))
;;       :bind
;;       (("C-x C-h u" . hydra-undo-tree/body))
;;       :custom
;;       (undo-tree-auto-save-history nil))
;;   (use-package undo-tree
;;     :ensure t
;;     :config 
;;     (global-undo-tree-mode)))

(global-set-key (kbd "C-x u") 'undo-tree-visualize)
(global-set-key (kbd "C-/") 'undo-tree-undo)


(require 'init-modeline)

(use-package term
  :config
  (define-key term-raw-map (kbd "C-c C-g") 'swk/kill-process-and-window))

(when (display-graphic-p)
   (require 'init-latex)
;;   (require 'init-chinese-word-segment)
;;   (require 'init-iterm)
  ;; (use-package good-scroll
  ;; 	:ensure t
  ;; 	:if window-system
  ;; 	:init (good-scroll-mode))
  )

(use-package google-this
  :if (display-graphic-p)
  :ensure t
  :init
  (google-this-mode)
  :config
  (global-set-key (kbd "C-c g") 'google-this))

(use-package evil
  :disabled
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (evil-local-mode 1)))
  (add-hook 'org-mode-hook (lambda () (evil-local-mode 1)))
  (add-hook 'markdown-mode-hook (lambda () (evil-local-mode 1)))(defun my-evil-escape ()
  "Change 'jj' to ESC in evil-insert-state."
  (interactive)
  (insert "j")
  (if (equal (buffer-substring (- (point) 2) (point)) "jj")
      (progn
        (delete-char -2)
        (evil-normal-state))))
  (define-key evil-insert-state-map "j" 'my-evil-escape)) 

;; (define-key evil-insert-state-map "j" 'my-evil-escape)


(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

; (require 'mdx-dictionary)


(use-package editorconfig
  :ensure t)
(use-package jsonrpc
  :ensure t)




;;(when (display-graphic-p)
;;   (use-package chatgpt-shell
;; 	:custom
;; 	((chatgpt-shell-openai-key
;;       (lambda ()
;; 		;; Here the openai-key should be the proxy service key.
;; 		(pv/osx-get-keychain-password "openai key"))))))

(provide 'init)

;;; init.el ends here
