;;; init-markdown.el -- markdown support for Emacs

;;; Commentary:
;;; use markdown package to add markdown to Emacs

;;; Code:
(use-package markdown-mode
  :init
  (autoload 'instant-markdown-mode "instant-markdown")
  (add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  :mode ("\\.md\\'" . gfm-mode))

(use-package instant-markdown
  :commands (instant-markdown-mode)
  :ensure nil
  :load-path (lambda () (expand-file-name "site-lisp" user-emacs-directory)))

(provide 'init-markdown)
;;; init-markdown.el ends here
