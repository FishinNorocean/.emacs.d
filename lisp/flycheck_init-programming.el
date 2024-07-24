;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     lsp-mode and dap-mode should be installed and loaded first.
;;; Code:

;; basic
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

(require 'dap-ui)
(require 'dap-mouse)

(use-package dap-mode  
  :ensure t  
  :after lsp-mode  
  :custom  
  (dap-auto-configure-mode t)  
  :commands (dap-debug)  
  :hook  
  ((python-mode . dap-ui-mode)  
   (python-mode . dap-mode)  
   (python-mode . tooltip-mode)  
   (python-mode . dap-tooltip-mode)  
   (python-mode . dap-ui-controls-mode))  
  :config  
  (require 'dap-python)  
  (setq dap-python-debugger 'debugpy)  
  :bind  
  (:map dap-mode-map  
        ("C-c d d" . dap-debug)  
        ("C-c d b" . dap-breakpoint-toggle)  
        ;; ("C-c d c" . dap-continue)  
        ;; ("C-c d n" . dap-next)  
        ;; ("C-c d i" . dap-step-in)  
        ;; ("C-c d o" . dap-step-out)  
        ("C-c d q" . dap-disconnect)  
        ("C-c d r" . dap-restart-frame)  
        ("C-c d e" . dap-eval)  
        ("C-c d r" . dap-eval-region))  
  )


;; C/C++
(use-package c++-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state)
  :bind
  ("C-c o" . ff-find-other-file)
  ;; ("C-c o" . ff-find-other-file-other-window)
)

(use-package clang-format
  :ensure t)

(use-package cmake-mode
  :ensure t)

;; debug
(use-package dap-cpptools
  :after dap-mode)

(when *is-a-mac*
  (use-package dap-lldb
										;:disabled
	:after dap-mode
	:custom
	(dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
	;; ask user for executable to debug if not specified explicitly (c++)
	(dap-lldb-debugged-program-function
	 (lambda () (read-file-name "Select file to debug: "))))
  ;; default debug template for (c++)
  ;; (dap-register-debug-template
  ;;  "LLDB:vscode"
  ;;  (list :type "lldb-vscode"
  ;;        :cwd nil
  ;;        :args nil
  ;;        :request "launch"
  ;;        :program nil)))
  )

;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  ;:interpreter ("~/.local/opt/miniconda3/bin/python3" . python-mode)
  )


(use-package pyvenv
  :ensure t
  :disabled
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/.local/opt/miniconda3/envs"))
  ;; (setq python-shell-interpreter "python3")
  (pyvenv-mode t)
  :hook
  (python-mode . (lambda () (pyvenv-workon "numpy"))))

(use-package conda
  :ensure t
  :custom
  (conda-anaconda-home (expand-file-name "~/.local/opt/miniconda3"))
  :config
  (conda-env-initialize-eshell)
  (conda-env-initialize-interactive-shells)
  (setq conda-mode-line '(:propertize
						  (:eval (when conda-env-current-name
								   (concat "[" conda-env-current-name "]")))
						  help-echo "Current conda env"))
  (conda-mode-line-setup)
  (conda-env-activate "cpws")
  )


(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
				   (require 'lsp-pyright)
				   (lsp-deferred))))

;; Rust
(use-package rust-mode
  :ensure t
  :functions dap-register-debug-template
  ;; :bind
  ;; ("C-c C-c" . rust-run)
  :hook
  (rust-mode . lsp-deferred)
  :config
  ;; debug
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb-vscode"
									 :request "launch"
									 :name "rust-lldb::Run"
									 ;; :gdbpath "rust-lldb"
									 :target nil
									 :cwd nil)))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

;; For ns-3
;; (load-file (expand-file-name "~/.emacs.d/mymode/ns3-mode.el"))
;; (require 'ns3-mode)

;; Print ANSI colors in compilation mode buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(provide 'init-programming)
;;; init-programming.el ends here
