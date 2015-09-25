;;; init-javascript.el -- javascript setup

;;; Commentary:
;;; js2 mode setup and related stuff

;;; Code:
(require-package 'js2-mode)
(require-package 'tern)
(require-package 'company-tern)
(require-package 'js2-refactor)

(require 'js2-mode)
(require 'js2-refactor)

(setq js-indent-level 2)
(js2r-add-keybindings-with-prefix "C-c C-r")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook #'tern-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(diminish 'js2-refactor-mode)

(provide 'init-javascript)
;;; init-javascript.el ends here
