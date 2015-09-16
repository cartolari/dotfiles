;;; init-company.el -- Company mode setup

;;; Commentary:
;;; company setup

;;; Code:
(require-package 'company)
(require-package 'company-quickhelp)

(company-quickhelp-mode 1)
(global-company-mode 1)

(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(global-set-key (kbd "M-/") 'hippie-expand)
(add-hook 'company-completion-started-hook (lambda (arg) (fci-mode 0)))
(add-hook 'company-completion-finished-hook (lambda (arg) (fci-mode 1)))

(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case t)
(setq company-idle-delay 0.25)
(setq company-minimum-prefix-length 0)

(diminish 'company-mode)

(provide 'init-company)
;;; init-company.el ends here
