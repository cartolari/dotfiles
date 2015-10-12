;;; init-yaml.el -- yaml mode setup

;;; Commentary:
;;; load yaml mode

;;; Code:
(use-package yaml-mode)

(require 'outline)
(require 'yaml-mode)

(add-hook 'yaml-mode-hook (lambda ()
                            (setq evil-shift-width yaml-indent-offset)))
(add-hook 'yaml-mode-hook
          (lambda () (progn
                       (outline-minor-mode 1)
                       (setq outline-regexp " *")
                       (local-set-key (kbd "<C-return>")
                                      'outline-toggle-children))))

(add-to-list 'company-dabbrev-code-modes 'yaml-mode)

(diminish 'outline-minor-mode " ☰")

(provide 'init-yaml)

;;; init-yaml.el ends here
