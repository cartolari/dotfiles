;;; init-company.el -- Company mode setup

;;; Commentary:
;;; company setup

;;; Code:
(use-package company
  :defer t
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (add-hook 'company-completion-cancelled-hook (lambda (arg) (fci-mode 1)))
  (add-hook 'company-completion-finished-hook (lambda (arg) (fci-mode 1)))
  (add-hook 'company-completion-started-hook (lambda (arg) (fci-mode 0)))
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-idle-delay 0.25)
  (setq company-minimum-prefix-length 0)
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode))
(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))
(use-package company-try-hard
  :defer t
  :init
  (autoload 'company-try-hard "company-try-hard" nil t)
  (global-set-key (kbd "C-'") #'company-try-hard))
(use-package company-ycmd
  :defer t
  :config
  (company-ycmd-setup)
  (setq ycmd--log-enabled t)
  :diminish ycmd-mode
  :init
  (add-hook 'after-init-hook #'global-ycmd-mode)
  (setq ycmd-server-command '("python2" "/home/bruno/code/ycmd/ycmd")))
(use-package readline-complete)

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-company)
;;; init-company.el ends here
