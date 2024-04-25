;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;; This file contains pre-defined funcitons.
;;; Code:

(define-obsolete-function-alias 'after-load 'with-eval-after-load "")

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Like diminish, but for major modes. To show mode name in mode line more coherently
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

(defun kill-buffer-if-exists (buffer-name)
  "Kill the buffer with the name BUFFER-NAME if it exists."
  (let ((buffer (get-buffer buffer-name)))
    (when buffer
      (kill-buffer buffer))))

;; String utilities missing from core emacs

(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;; Delete the current file

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; Rename the current file

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun swk/touchfile (new-file-name)
  "Touch file NEW-FILE-NAME."
  (interactive "sTouch what: ")
  (shell-command (concat "touch " new-file-name)))

;; Browse current HTML file

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defvar swk/quick-access-urls
  '(("h" . "https://www.zjuers.com")
    ("y" . "https://www.youtube.com")
    ("m" . "https://www.gmail.com")
	("o" . "https://copilot.microsoft.com/?wlexpsignin=1")
	("g" . "https://www.github.com")
	("b" . "https://www.bilibili.com")
	("c" . "https://www.cc98.org"))
  "Predefined shortcut options.")

(defun swk/web-quick-access ()
  "Help you launch browser quicker.
a for quick access, u for goto url, g for 'google=this."
  (interactive)
  (let ((key (read-key-sequence "Choose an option(a|u|g): ")))
    (cond ((string= key "a")
           (let ((buffer (generate-new-buffer "*Shortcut-Options*"))
                 (options-window (split-window-below (floor (* 0.5 (window-height))))))
             (with-current-buffer buffer
               (mapc (lambda (site)
                       (insert (format "%s: %s\n" (car site) (cdr site))))
                     swk/quick-access-urls))
             (set-window-buffer options-window buffer)
             (catch 'done
               (while t
                 (let* ((choice (read-key-sequence "Choose your shoutcut or press 'q' to quit: "))
                        (url (if (string= choice "q") (progn (kill-buffer buffer) (delete-window options-window) (throw 'done t)) (cdr (assoc choice swk/quick-access-urls)))))
                   (if url
                       (progn
                         (browse-url url)
                         (kill-buffer buffer)
                         (delete-window options-window)
                         (throw 'done t))
                     (unless (string= choice "q")
                       (beep))))))))
          ((string= key "u")
           (call-interactively 'browse-url))
          ((string= key "g")
           (call-interactively 'google-this)))))

(defun swk/kill-process-and-window ()
  "Kill a term or console."
  (interactive)
   (let ((process (get-buffer-process (current-buffer))))
    (if process
      (progn
        (kill-buffer-and-window))
    (message "It's not a process!"))))

(defun copy-whole-line ()
  "Copy the whole line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
  (message "1 line copied"))

(defun file-name-only ()
  "Get the current buffer file name without directory."
  (file-name-nondirectory (buffer-name)))

(defun file-name-only-noext ()
  "Get the currennt buffer file name without directory and extension."
  (file-name-sans-extension (file-name-only)))

;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))

;; pretty paste and copy
(unless (display-graphic-p)
  (defun pbpaste ()
	"Paste data from pasteboard."
	(interactive)
	(shell-command-on-region
	 (point)
	 (if mark-active (mark) (point))
	 "pbpaste" nil t))

  (defun pbcopy ()
	"Copy region to pasteboard."
	(interactive)
	(print (mark))
	(when mark-active
	  (shell-command-on-region
	   (point) (mark) "pbcopy")
	  (kill-buffer "*Shell Command Output*"))))

(defun toggle-window-split ()
  "Switch windows-spliting between horizontally and vertically."
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
						     (car next-win-edges))
						 (<= (cadr this-win-edges)
						     (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)

		  (if this-win-2nd (other-window 1))))))

(defun ansi-term-zsh ()
  "Launch zsh with 'ansi-term."
  (interactive)
  (ansi-term "/bin/zsh"))

(defun swk/new-terminal ()
  "Open shell in other window."
  (interactive)
  ; (split-window-right)
  ; (other-window 1)
  (ansi-term "/bin/zsh"))

(defun list-buffers-other-window()
  "List buffers and swith the cursor to the list window."
  (interactive)
  (list-buffers)
  (other-window 1))

;; (when *is-a-mac*
;;   (defun pv/osx-get-keychain-password (account-name)
;; 	"Gets ACCOUNT-NAME keychain password from OS X Keychain."
;; 	(let ((cmd (concat "security 2>&1 >/dev/null find-generic-password -ga '" account-name "'")))
;; 	  (let ((passwd (shell-command-to-string cmd)))
;; 		(when (string-match (rx "\"" (group (0+ (or (1+ (not (any "\"" "\\"))) (seq "\\" anything)))) "\"") passwd)
;; 		  (match-string 1 passwd))))))

;; SSH remote
(defun connect-cec-erver ()
  (interactive)
  (dired "/ssh:cec18053:"))
;; (defun connect-ubuntu ()
;;   (interactive)
;;   (dired "/ssh:pavin@172.16.172.133:/home/pavin/Code/"))
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(provide 'init-utils)
;;; init-utils.el ends here
