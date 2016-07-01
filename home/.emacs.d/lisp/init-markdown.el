;;; init-markdown.el -- markdown support for Emacs

;;; Commentary:
;;; use markdown package to add markdown to Emacs

;;; Code:
(require 'use-package)

(use-package markdown-mode
  :init
  (autoload 'instant-markdown-mode "instant-markdown")
  (add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  :mode ("\\.md\\'" . gfm-mode))

(use-package vmd-mode
  :commands (vmd-mode))

(provide 'init-markdown)
;;; init-markdown.el ends here
