;;; init-go.el -- Go programming language support for Emacs

;;; Commentary:
;;; go setup

;;; Code:
(require 'use-package)

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (setq gofmt-command "goimports")
  (evil-define-key 'normal go-mode-map "gd" 'godef-jump)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-eldoc
  :commands (go-eldoc-setup)
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-rename
  :commands (go-rename))

(provide 'init-go)
;;; init-go.el ends here
