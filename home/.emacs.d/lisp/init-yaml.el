;;; init-yaml.el -- yaml mode setup

;;; Commentary:
;;; load yaml mode

;;; Code:
(require 'use-package)

(use-package outline
  :commands (outline-mode outline-minor-mode)
  :diminish (outline-minor-mode . "☰"))

(use-package yaml-mode
  :config
  (bind-key "<C-return>" 'outline-toggle-children yaml-mode-map)
  :init
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq evil-shift-width yaml-indent-offset)))
  (add-hook 'yaml-mode-hook 'outline-minor-mode)
  (add-hook 'yaml-mode-hook (lambda () (setq outline-regexp " *")))
  (with-eval-after-load 'company
    '(add-to-list 'company-dabbrev-code-modes 'yaml-mode)))

(use-package ansible-doc
  :commands (ansible-doc ansible-doc-mode)
  :diminish ansible-doc-mode)

(provide 'init-yaml)
;;; init-yaml.el ends here
