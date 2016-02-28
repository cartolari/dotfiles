;;; init-sh.el -- shell mode setup

;;; Commentary:
;;; shell mode setup

;;; Code:
(require 'use-package)

(use-package sh-script
  :config
  (setq-default sh-basic-offset 2
                sh-indentation 2)
  (add-hook 'sh-mode-hook
            (lambda()
              (setq evil-shift-width sh-indentation)))
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)
  :ensure nil)

(use-package company-shell
  :commands (company-shell-mode)
  :init
  (add-hook 'sh-mode-hook 'company-shell-mode))

(provide 'init-sh)
;;; init-sh.el ends here
