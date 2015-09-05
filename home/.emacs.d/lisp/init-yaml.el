;;; init-yaml.el -- yaml mode setup

;;; Commentary:
;;; load yaml mode

;;; Code:
(require-package 'yaml-mode)

(require 'yaml-mode)

(provide 'init-yaml)

(add-hook 'yaml-mode-hook (lambda () (electric-indent-local-mode -1)))

;;; init-yaml.el ends here
