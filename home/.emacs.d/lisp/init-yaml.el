;;; init-yaml.el -- yaml mode setup

;;; Commentary:
;;; load yaml mode

;;; Code:
(require-package 'yaml-mode)

(require 'yaml-mode)

(add-hook 'yaml-mode-hook (lambda () (setq evil-shift-width yaml-indent-offset)))
(add-hook 'yaml-mode-hook (lambda () (setq outline-regexp " *")))
(add-hook 'yaml-mode-hook 'outline-minor-mode)

(provide 'init-yaml)

;;; init-yaml.el ends here
