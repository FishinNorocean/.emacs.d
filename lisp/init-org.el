;;; init-org.el -- Org mode setup
;;; Commentary:
;;;    

;;; Code:



(use-package org
  :config
  ;; (defun swk/init-org-hook ()
  ;; 	(org-toggle-pretty-entities))
  
  
	
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview t)
  (setq org-image-actual-width '(500))
  
  
  :hook
  (org-mode-hook . (lambda () (visual-line-mode 1)))
  (org-mode-hook . yas-minor-mode)
  (org-mode-hook . flyspell-mode)
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


(defun swk/pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  ;(org-agenda-list)
  (org-agenda nil "d")
  (when (not split)
    (delete-other-windows)))

(define-key global-map (kbd "C-c a") 'org-agenda)

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
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

(setq org-capture-templates
	  '(("a" "My TODO task format." entry
         (file "~/org/Agenda/todo.org")
         "* TODO %?
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

(provide 'init-org)

;;; init-org.el ends here
