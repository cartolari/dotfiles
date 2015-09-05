;;; init-company.el -- Company mode setup

;;; Commentary:
;;; company setup

;;; Code:
(require-package 'company)
(require-package 'company-quickhelp)
(require-package 'company-ycmd)

(require 'ycmd)

(company-quickhelp-mode 1)
(company-ycmd-setup)
(global-company-mode 1)

(add-hook 'after-init-hook #'global-ycmd-mode)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(set-variable 'ycmd-server-command '("python" "/home/bruno/code/ycmd/ycmd"))

(diminish 'company-mode)
(diminish 'ycmd-mode)

(provide 'init-company)
;;; init-company.el ends here
