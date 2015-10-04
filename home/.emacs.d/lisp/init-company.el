;;; init-company.el -- Company mode setup

;;; Commentary:
;;; company setup

;;; Code:
(require-package 'company)
(require-package 'company-quickhelp)
(require-package 'company-try-hard)
(require-package 'company-ycmd)
(require-package 'readline-complete)

(require 'company-ycmd)
(require 'company-dabbrev)

(company-quickhelp-mode 1)
(global-company-mode 1)
(company-ycmd-setup)

(add-hook 'after-init-hook #'global-ycmd-mode)
(add-hook 'company-completion-cancelled-hook (lambda (arg) (fci-mode 1)))
(add-hook 'company-completion-finished-hook (lambda (arg) (fci-mode 1)))
(add-hook 'company-completion-started-hook (lambda (arg) (fci-mode 0)))

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-'") #'company-try-hard)
(define-key company-active-map (kbd "C-'") #'company-try-hard)

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-idle-delay 0.25)
(setq company-minimum-prefix-length 0)

(set-variable 'ycmd-server-command '("python" "/home/bruno/code/ycmd/ycmd"))
(setq ycmd-tag-files "/home/bruno/code/vizir/safeguard/.git/tags")
(setq ycmd--log-enabled t)

(diminish 'company-mode)
(diminish 'ycmd-mode)

(provide 'init-company)
;;; init-company.el ends here
