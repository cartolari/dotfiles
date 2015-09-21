;;; init-markdown.el -- markdown support for Emacs

;;; Commentary:
;;; use markdown package to add markdown to Emacs

;;; Code:
(require-package 'markdown-mode)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'evil-local-mode)

(provide 'init-markdown)
;;; init-markdown.el ends here
