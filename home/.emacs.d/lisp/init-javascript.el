;;; init-javascript.el -- javascript setup

;;; Commentary:
;;; js2 mode setup and related stuff

;;; Code:
(use-package js2-mode
  :config
  (setq js-indent-level 2)
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'tern-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :interpreter ("node" . js2-mode))
(use-package tern
  :commands (tern-mode)
  :config
  (use-package company-tern)
  (add-to-list 'company-backends 'company-tern))
(use-package js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  :commands (js2-refactor-mode)
  :diminish js2-refactor-mode)

(provide 'init-javascript)
;;; init-javascript.el ends here
