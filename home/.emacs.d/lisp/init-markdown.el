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
  :defer t
  :ensure nil
  :init
  (autoload 'instant-markdown-mode "instant-markdown" nil t))


(provide 'init-markdown)
;;; init-markdown.el ends here
