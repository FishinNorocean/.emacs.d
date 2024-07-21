;;; init-org.el -- Org mode setup
;;; Commentary:
;;;    

;;; Code:

(use-package captain
  :ensure t
  :config
  (add-hook 'prog-mode-hook
			(lambda ()
			  (setq captain-predicate (lambda () (nth 8 (syntax-ppss (point)))))))
  (add-hook 'text-mode-hook
			(lambda ()
              (setq captain-predicate (lambda () t))))

  (add-hook
   'org-mode-hook
   (lambda ()
	 (setq captain-predicate
           (lambda () (not (org-in-src-block-p)))))))

(use-package org
  :config
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
								"xelatex -interaction nonstopmode %f"))
  (setq org-latex-default-packages-alist
		(remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
  ;; (defun swk/init-org-hook ()
  ;; 	(org-toggle-pretty-entities))
  
  
	
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t)
  (setq org-image-actual-width '(500))
  
  
  :hook
  (org-mode-hook . (lambda () (visual-line-mode 1)))
  (org-mode-hook . (lambda () (company-mode 0)))
  (org-mode-hook . yas-minor-mode)
  (org-mode-hook . flyspell-mode)
  (org-mode-hook . captain-mode)
  (org-mode-hook . (lambda ()
					 (setq-local electric-pair-inhibit-predicate
								 `(lambda (c)
									(if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
  ;; (org-mode-hook . (lambda ()
  ;;               (define-key python-mode-map "\"" 'electric-pair)
  ;;               (define-key python-mode-map "\'" 'electric-pair)
  ;;               (define-key python-mode-map "(" 'electric-pair)
  ;;               (define-key python-mode-map "[" 'electric-pair)
  ;;               (define-key python-mode-map "{" 'electric-pair)))
  ;; (org-mode-hook . disable-angle-bracket-in-org-mode)
  ;; (org-mode-hook . swk/init-org-hook)
  )

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'flyspell-mode)
  (add-hook 'org-mode-hook
			  (lambda () (local-set-key (kbd "C-j") nil)))
  (setq-local electric-pair-pairs (remq '(?< . ?>) electric-pair-pairs)))
;; Agenda settings:
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
(setq org-agenda-files '("~/org/Agenda/"))


(defun swk/pop-to-org-agenda-day ()
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive)
  ;(org-agenda-list)
  (org-agenda nil "d")
  ;; (when (not split)
  ;;   (delete-other-windows))
  )

(defun swk/pop-to-org-agenda-simple ()
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive)
  ;(org-agenda-list)
  (org-agenda nil "c")
  ;; (when (not split)
  ;;   (delete-other-windows))
  )

(define-key global-map (kbd "C-c a") 'swk/pop-to-org-agenda-simple)
;; (define-key global-map (kbd "C-c d") 'swk/pop-to-org-agenda-day)

(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))


(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (air-org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))
		("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span 'day)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ;((org-agenda-compact-blocks t))
		 )))

(setq org-capture-templates
	  '(("a" "My TODO task format." entry
         (file "~/org/Agenda/todo.org")
         "
* TODO %?
SCHEDULED: %t")))
(setq org-agenda-text-search-extra-files '(agenda-archives))
(setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
(setq org-enforce-todo-dependencies t)
(setq org-log-done (quote time))
(setq org-log-redeadline (quote time))
(setq org-log-reschedule (quote time))



(defun swk/org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))
(define-key global-map (kbd "C-c c") 'swk/org-task-capture)


(use-package org-fragtog
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

(use-package org-roam
  :disabled
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
         ;; Dailies
         ;; ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-export)
  (org-roam-db-autosync-mode))

  ;; If using org-roam-protocol
  ;;(require 'org-roam-protocol))


(provide 'init-org)

;;; init-org.el ends here
