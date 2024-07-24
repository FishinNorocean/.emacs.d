;;; init-chinese.el --- Chinese support
;;; Commentary:
;;;     Chinese words seperator support added with jieba

;;; Code:

(use-package cns
  :custom
  (cns-prog (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"))
  (cns-dict-directory (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"))
  :hook
  (text-mode . cns-mode)
  :config
  (setq cns-debug nil))
;(setq cns-prog (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cnws"))
;(setq cns-dict-directory (expand-file-name "~/.emacs.d/site-lisp/emacs-chinese-word-segmentation/cppjieba/dict"))

;(setq cns-debug nil) ; disable debug output, default is t


(provide 'init-chinese)
;;; init-chinese.el ends here
